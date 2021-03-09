package se.kth.id2203.consensus

import se.kth.id2203.kvstore.Operation
import se.kth.id2203.networking.NetAddress

case class Command(client: NetAddress, op: Operation)
