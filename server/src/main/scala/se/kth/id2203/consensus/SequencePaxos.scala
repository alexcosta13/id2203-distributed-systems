package se.kth.id2203.consensus

import se.kth.id2203.ballotleaderelection.{BLE_Leader, BallotLeaderElection}
import se.kth.id2203.consensus.Role._
import se.kth.id2203.consensus.State._
import se.kth.id2203.networking.{NetAddress, NetMessage}
import se.kth.id2203.overlay.{TopologyProvider, TopologyMsg}

import scala.collection.mutable
import se.sics.kompics.sl._
import se.sics.kompics.network.Network

class SequencePaxos() extends ComponentDefinition {

  //******* Ports ******
  val sc: NegativePort[SequenceConsensus.type] = provides(SequenceConsensus)
  val ble: PositivePort[BallotLeaderElection.type] = requires(BallotLeaderElection)
  val net: PositivePort[Network] = requires[Network]
  val topology: PositivePort[TopologyProvider.type] = requires(TopologyProvider)

  //******* Fields ******
  val self: NetAddress = cfg.getValue[NetAddress]("id2203.project.address")
  var nodes: Set[NetAddress] = Set()
  var others: Set[NetAddress] = Set()
  val delta: Long = cfg.getValue[Long]("id2203.project.delta")
  var majority: Int = 0

  var state: (Role, State) = (FOLLOWER, UNKNOWN)
  var nL = 0L
  var nProm = 0L
  var leader: Option[NetAddress] = None
  var na = 0L
  var va = List.empty[Command]
  var ld = 0
  // leader state
  var propCmds = List.empty[Command]
  val las = mutable.Map.empty[NetAddress, Int]
  val lds = mutable.Map.empty[NetAddress, Int]
  var lc = 0
  val acks = mutable.Map.empty[NetAddress, (Long, List[Command])]

  //******* Functions ******
  def suffix(s: List[Command], l: Int): List[Command] = {
    s.drop(l)
  }

  def prefix(s: List[Command], l: Int): List[Command] = {
    s.take(l)
  }

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start =>  {
    }
  }

  topology uponEvent {
    case TopologyMsg(topology) => {
      log.debug(s"Paxos gets topology.")
      nodes = topology
      majority = (nodes.size / 2) + 1
      others = nodes - self
    }
  }

  ble uponEvent {
    case BLE_Leader(l, n) => {
      if (n > nL) {
        leader = Some(l)
        nL = n
        if (self == l && nL > nProm) {
          state = (LEADER, PREPARE)
          propCmds.empty
          las.empty
          lds.empty
          acks.empty
          lc = 0
          for(p <- others) {
            trigger(NetMessage(self, p, Prepare(nL, ld, na)) -> net)
          }
          acks += (l -> (na, suffix(va, ld)))
          lds += (self -> ld)
          nProm = nL
        } else {
          state = (FOLLOWER, state._2)
        }
      }
    }
  }

  net uponEvent {
    case NetMessage(header, Prepare(np, ldp, n)) => {
      if (nProm < np) {
        nProm = np
        state = (FOLLOWER, PREPARE)
        var sfx = List.empty[Command]
        if (na >= n) {
          sfx = suffix(va, ldp)
        }
        trigger(NetMessage(self, header.src, Promise(np, na, sfx, ld)) -> net)
      }
    }
    case NetMessage(header, Promise(n, na, sfxa, lda)) => {
      val a = header.src
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks += (a -> (na, sfxa))
        lds += (a -> lda)
        if (acks.size >= majority) {
          var (k, sfx) = acks.values.reduce((ack1, ack2) => {
            if (ack1._1 == ack2._1) {
              if (ack1._2.size >= ack2._2.size) ack1 else ack2
            } else if (ack1._1 > ack2._1) {
              ack1
            } else {
              ack2
            }
          })
          va = prefix(va, ld) ++ sfx ++ propCmds
          las += (self -> va.size)
          propCmds.empty
          state = (LEADER, ACCEPT)
          for (p <- lds.keys) {
            if(p != self) {
              val sfxp = suffix(va, lds(p))
              trigger(NetMessage(self, p, AcceptSync(nL, sfxp, lds(p))) -> net)
            }
          }
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds += (a -> lda)
        val sfx = suffix(va, lds(a))
        trigger(NetMessage(self, a, AcceptSync(nL, sfx, lds(a))) -> net)
        if (lc != 0) {
          trigger(NetMessage(self, a, Decide(ld, nL)) -> net)
        }
      }
    }
    case NetMessage(header, AcceptSync(nL, sfx, ldp)) => {
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
        na = nL
        va = prefix(va, ldp) ++ sfx
        trigger(NetMessage(self, header.src, Accepted(nL, va.size)) -> net)
        state = (FOLLOWER, ACCEPT)
      }
    }
    case NetMessage(header, Accept(nL, c)) => {
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
        va = va ++ List(c)
        trigger(NetMessage(self, header.src, Accepted(nL, va.size)) -> net)
      }
    }
    case NetMessage(_, Decide(l, nL)) => {
      if (nProm == nL) {
        while (ld < l) {
          trigger(SC_Decide(va(ld)) -> sc)
          ld += 1
        }
      }
    }
    case NetMessage(header, Accepted(n, m)) => {
      if ((n == nL) && (state == (LEADER, ACCEPT))) {
        las += (header.src -> m)
        val count = las.values.count(_ >= m)
        if (lc < m && count >= majority) {
          lc = m
          for (p <- lds.keys) {
            trigger(NetMessage(self, p, Decide(lc, nL)) -> net)
          }
        }
      }
    }
  }

  sc uponEvent {
    case SC_Propose(c) => {
      if (state == (LEADER, PREPARE)) {
        propCmds = propCmds ++ List(c)
      }
      else if (state == (LEADER, ACCEPT)) {
        va = va ++ List(c)
        las += (self -> (las(self) + 1))
        for (p <- lds.keys) {
          if (p != self) {
            trigger(NetMessage(self, p, Accept(nL, c)) -> net)
          }
        }
      }
    }
  }
}