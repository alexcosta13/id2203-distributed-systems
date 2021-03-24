package se.kth.id2203.consensus

object Role extends Enumeration {
  type Role = Value
  val LEADER, FOLLOWER = Value
}
