/*
 * The MIT License
 *
 * Copyright 2017 Lars Kroll <lkroll@kth.se>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package se.kth.id2203.simulation

import se.kth.id2203.kvstore._
import se.kth.id2203.networking._
import se.kth.id2203.overlay.RouteMsg
import se.sics.kompics.Start
import se.sics.kompics.network.Network
import se.sics.kompics.sl._
import se.sics.kompics.sl.simulator.SimulationResult
import se.sics.kompics.timer.Timer

import java.util.UUID
import scala.collection.mutable

class LinClient extends ComponentDefinition {

  //******* Ports ******
  val net: PositivePort[Network] = requires[Network]
  val timer: PositivePort[Timer] = requires[Timer]

  //******* Fields ******
  val self: NetAddress = cfg.getValue[NetAddress]("id2203.project.address")
  val server: NetAddress = cfg.getValue[NetAddress]("id2203.project.bootstrap-address")
  private val pending = mutable.Map.empty[UUID, String]
  var history = Array.empty[Any]

  //******* Functions ******
  def send(op: Operation): Unit = {
    val routeMsg = RouteMsg(op.key, op); // don't know which partition is responsible, so ask the bootstrap server to forward it
    trigger(NetMessage(self, server, routeMsg) -> net)
    pending += (op.id -> op.key)
    history ++= List(op)
    logger.info("Sending {}", op)
    SimulationResult += (op.key -> "Sent")
  }

  def isLinearizable(h: Array[Any], S: mutable.Map[String, String]): Boolean = {
    if(h.isEmpty) return true
    for(op <- h) {
      op match {
        case operation: Operation =>
          val res = h.findLast(
            o => o.isInstanceOf[OpResponse] &&
              o.asInstanceOf[OpResponse].id == operation.id
          ).get.asInstanceOf[OpResponse]
          if (equivalentResponse(res, perform(operation, S)) &&
            isLinearizable(h.filter(o => (o.isInstanceOf[OpResponse] && o.asInstanceOf[OpResponse] != res)  ||
              (o.isInstanceOf[Operation] && o.asInstanceOf[Operation] != operation)), S)) {
            return true
          } else {
            undo(operation, S)
          }
        case _ =>
      }
    }
    false
  }

  def perform(op: Operation, S: mutable.Map[String, String]): OpResponse = {
    op match {
      case op @ Get(key, _) => {
        if (S.contains(key)) {
          val value = S(key)
          op.response(OpCode.Ok, value)
        } else {
          op.response(OpCode.NotFound)
        }
      }
      case op @ Put(key, value, _) => {
        S += (key -> value)
        op.response(OpCode.Ok)
      }
      case op @ Cas(key, refValue, newValue, _) => {
        if (S.contains(key) && S(key).equals(refValue)) {
          S += (key -> newValue)
          op.response(OpCode.Ok, refValue)
        } else {
          op.response(OpCode.NotFound)
        }
      }
    }
  }

  def undo(op: Operation, S: mutable.Map[String, String]): Unit = {
    op match {
      case _: Get =>
      case op @ Put(key, _, _) => {
        if (S.contains(key)) S.remove(op.key)
      }
      case op @ Cas(key, refValue, newValue, _) => {
        if (S.contains(key) && S(key) == newValue) {
            S(op.key) = refValue
        }
      }
    }
  }

  def equivalentResponse(actual: OpResponse, expected: OpResponse): Boolean = {
    actual.status == expected.status && actual.value == expected.value
  }

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      history = Array.empty[Any]
      send(Put(s"test1", "value1"))
      send(Get(s"test1"))
      send(Cas(s"test1", "value1", "newValue1"))
      send(Get(s"missingKey"))
      send(Get(s"test1"))
      send(Put(s"test1", "otherValue1"))
      send(Get(s"test1"))
      send(Cas(s"test1", "newValue1", "")) // should not change value
    }
  }

  net uponEvent {
    case NetMessage(_, or @ OpResponse(id, _, _)) => {
      logger.debug(s"Got OpResponse: $or")
      pending.remove(id) match {
        case Some(_) => history ++= List(or)
        case None      => logger.warn("ID $id was not pending! Ignoring response.");
      }
      if (pending.isEmpty) {
        SimulationResult += ("linearizable" -> isLinearizable(history, mutable.Map.empty[String,String]).toString)
      }
    }
  }
}
