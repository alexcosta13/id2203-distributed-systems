package se.kth.id2203.consensus

import se.kth.id2203.kvstore.Operation
import se.kth.id2203.networking.NetAddress
import se.sics.kompics.KompicsEvent

case class RSM_Command(client: NetAddress, op: Operation) extends KompicsEvent
