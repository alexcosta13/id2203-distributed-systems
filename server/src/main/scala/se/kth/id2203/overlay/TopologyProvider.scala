package se.kth.id2203.overlay

import se.sics.kompics.sl._

object TopologyProvider extends Port {
  indication[TopologyMsg]
}