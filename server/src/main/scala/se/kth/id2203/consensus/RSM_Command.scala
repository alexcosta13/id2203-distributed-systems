package se.kth.id2203.consensus

import se.kth.id2203.kvstore.Operation
import se.kth.id2203.networking.NetHeader
import se.sics.kompics.KompicsEvent

case class RSM_Command(header: NetHeader, op: Operation) extends KompicsEvent
