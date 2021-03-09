package se.kth.id2203.consensus

object State extends Enumeration {
  type State = Value
  val PREPARE, ACCEPT, UNKNOWN = Value
}