package se.kth.id2203.ballotleaderelection

import se.kth.id2203.networking.{NetAddress, NetMessage}
import se.kth.id2203.overlay.{TopologyProvider, TopologyMsg}

import scala.collection.mutable
import se.sics.kompics.sl._
import se.sics.kompics.network.Network
import se.sics.kompics.timer._

class GossipLeaderElection() extends ComponentDefinition {

  //******* Ports ******
  val ble: NegativePort[BallotLeaderElection.type] = provides(BallotLeaderElection)
  val net: PositivePort[Network] = requires[Network]
  val timer: PositivePort[Timer] = requires[Timer]
  val topology: PositivePort[TopologyProvider.type] = requires(TopologyProvider)

  //******* Fields ******
  val self: NetAddress = cfg.getValue[NetAddress]("id2203.project.address")
  var nodes: Set[NetAddress] = Set()
  val delta: Long = cfg.getValue[Long]("id2203.project.delta")
  var majority: Int = 0

  private var period = delta
  private val ballots = mutable.Map.empty[NetAddress, Long]

  private var round = 0L

  private var ballot = ballotFromNAddress(0, self)

  private var leader: Option[(Long, NetAddress)] = None
  private var highestBallot: Long = ballot

  //******* Functions ******
  private def ballotFromNAddress(n: Int, adr: NetAddress): Long = {
    val nBytes = com.google.common.primitives.Ints.toByteArray(n)
    val addrBytes = com.google.common.primitives.Ints.toByteArray(adr.hashCode())
    val bytes = nBytes ++ addrBytes
    val r = com.google.common.primitives.Longs.fromByteArray(bytes)
    assert(r > 0); // should not produce negative numbers!
    r
  }

  private def checkLeader(): Unit = {
    val (topProcess, topBallot) = (ballots ++ Map(self -> ballot)).maxBy { case (_, ballot) => ballot }
    val top = (topBallot, topProcess)
    if (topBallot < highestBallot) {
      while (ballot <= highestBallot) {
        ballot = incrementBallotBy(ballot, nodes.size)
      }
      leader = None
    } else {
      if (leader.isEmpty || (top != leader.get)) {
        highestBallot = topBallot
        leader = Some(top)
        log.info(s"New leader $topProcess, with ballot $topBallot")
        trigger(BLE_Leader(topProcess, topBallot) -> ble)
      }
    }
  }

  private def incrementBallotBy(ballot: Long, inc: Int): Long = {
    ballot + inc.toLong * 0x0100000000L
  }

  private def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(delay)
    scheduledTimeout.setTimeoutEvent(BLETimeout(scheduledTimeout))
    trigger(scheduledTimeout -> timer)
  }

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start =>  {
    }
  }

  topology uponEvent {
    case TopologyMsg(topology) => {
      log.debug(s"BLE gets topology...")
      nodes = topology
      majority = (nodes.size / 2) + 1
      startTimer(period)
    }
  }

  timer uponEvent {
    case BLETimeout(_) => {
      if (ballots.size + 1 >= majority) {
        checkLeader()
      }
      ballots.clear
      round += 1
      for (p <- nodes) {
        if (p != self) {
          trigger(NetMessage(self, p, HeartbeatReq(round, highestBallot)) -> net)
        }
      }
      startTimer(period)
    }
  }

  net uponEvent {
    case NetMessage(header, HeartbeatReq(r, hb)) => {
      if (hb > highestBallot) {
        highestBallot = hb
      }
      trigger(NetMessage(self, header.src, HeartbeatResp(r, ballot)) -> net)
    }
    case NetMessage(header, HeartbeatResp(r, b)) => {
      if (r == round) {
        ballots += ((header.src, b))
      } else {
        period += delta
      }
    }
  }
}
