package se.kth.id2203.consensus

import se.sics.kompics.sl._

object SequenceConsensus extends Port {
  request[SC_Propose]
  indication[SC_Decide]
}
