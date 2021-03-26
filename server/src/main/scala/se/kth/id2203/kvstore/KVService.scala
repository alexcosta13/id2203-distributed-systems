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
package se.kth.id2203.kvstore

import se.kth.id2203.consensus.{RSM_Command, SC_Decide, SC_Propose, SequenceConsensus}
import se.kth.id2203.networking._
import se.kth.id2203.overlay.Routing
import se.sics.kompics.sl._
import se.sics.kompics.network.Network

import scala.collection.mutable

class KVService extends ComponentDefinition {

  //******* Ports ******
  val net: PositivePort[Network] = requires[Network]
  val route: PositivePort[Routing.type] = requires(Routing) // needed for partitioning
  val consensus: PositivePort[SequenceConsensus.type] = requires(SequenceConsensus)

  //******* Fields ******
  val self: NetAddress = cfg.getValue[NetAddress]("id2203.project.address")
  var data: mutable.Map[String, String] = mutable.HashMap[String, String]()

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start =>  {
    }
  }

  net uponEvent {
    case NetMessage(header, op: Operation) => {
      log.info("Got operation {}!", op)
      trigger(SC_Propose(RSM_Command(header, op)) -> consensus)
    }
  }

  consensus uponEvent {
    case SC_Decide(RSM_Command(header, op @ Get(key, _))) => {
      log.info(s"Operation GET($key) decided.")
      if (data.contains(key)) {
        val value = data(key)
        if(header.dst == self) trigger(NetMessage(self, header.src, op.response(OpCode.Ok, value)) -> net)
      } else {
        if(header.dst == self) trigger(NetMessage(self, header.src, op.response(OpCode.NotFound)) -> net)
      }
    }

    case SC_Decide(RSM_Command(header, op @ Put(key, value, _))) => {
      log.info(s"Operation PUT($key -> $value) decided.")
      data += (key -> value)
      if(header.dst == self) trigger(NetMessage(self, header.src, op.response(OpCode.Ok)) -> net)
    }

    case SC_Decide(RSM_Command(header, op @ Cas(key, refValue, newValue, _))) => {
      log.info(s"Operation CAS(($key -> $newValue) if ($key -> $refValue)) decided.")
      if (data.contains(key) && data(key).equals(refValue)) {
        data += (key -> newValue)
        if(header.dst == self) trigger(NetMessage(self, header.src, op.response(OpCode.Ok, refValue)) -> net)
      } else {
        if(header.dst == self) trigger(NetMessage(self, header.src, op.response(OpCode.NotFound)) -> net)
      }
    }
  }
}
