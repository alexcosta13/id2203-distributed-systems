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

import java.util.UUID
import se.kth.id2203.kvstore._
import se.kth.id2203.networking._
import se.kth.id2203.overlay.RouteMsg
import se.sics.kompics.sl._
import se.sics.kompics.Start
import se.sics.kompics.network.Network
import se.sics.kompics.timer.Timer
import se.sics.kompics.sl.simulator.SimulationResult
import collection.mutable

class ScenarioClient extends ComponentDefinition {

  //******* Ports ******
  val net: PositivePort[Network] = requires[Network]
  val timer: PositivePort[Timer] = requires[Timer]

  //******* Fields ******
  val self: NetAddress = cfg.getValue[NetAddress]("id2203.project.address")
  val server: NetAddress = cfg.getValue[NetAddress]("id2203.project.bootstrap-address")
  private val pending = mutable.Map.empty[UUID, String]

  //******* Functions ******
  def get(messages: Int): Unit = {
    for (i <- 0 to messages) {
      send(Get(s"test$i"))
    }
  }

  def put(messages: Int): Unit = {
    for (i <- 0 to messages) {
      send(Put(s"test$i", s"value$i"))
    }
  }

  def cas(messages: Int, matchingKey: Boolean): Unit = {
    for (i <- 0 to messages) {
      val op = if (matchingKey) Cas(s"test$i", s"value$i", s"newValue$i")
      else Cas(s"test$i", s"v$i", s"newValue$i")
      send(op)
    }
  }

  def send(op: Operation): Unit = {
    val routeMsg = RouteMsg(op.key, op); // don't know which partition is responsible, so ask the bootstrap server to forward it
    trigger(NetMessage(self, server, routeMsg) -> net)
    pending += (op.id -> op.key)
    logger.info("Sending {}", op)
    SimulationResult += (op.key -> "Sent")
  }

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      val messages = SimulationResult[Int]("messages")
      SimulationResult[String]("operation") match {
         case "Get" => {
           if (SimulationResult[Boolean]("keyExists")) put(messages)
           get(messages)
         }
         case "Put" => {
           put(messages)
         }
         case "Cas" => {
           if (SimulationResult[Boolean]("keyExists")) put(messages)
           cas(messages, SimulationResult[Boolean]("keyMatches"))
           get(messages)
         }
       }
    }
  }

  net uponEvent {
    case NetMessage(header, or @ OpResponse(id, status, value)) => {
      logger.debug(s"Got OpResponse: $or")
      pending.remove(id) match {
        case Some(key) =>
          SimulationResult += (s"status$key" -> status.toString())
          SimulationResult += (s"value$key" -> value)
        case None      => logger.warn("ID $id was not pending! Ignoring response.");
      }
    }
  }
}
