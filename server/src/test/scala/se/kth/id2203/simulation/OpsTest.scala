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

import org.scalatest._

import se.sics.kompics.sl.simulator._
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.result.SimulationResultSingleton

class OpsTest extends FlatSpec with Matchers {

  private val nMessages = 10

  "Empty Get operation" should "return NotFound" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Get")
    SimulationResult += ("keyExists" -> false)
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("NotFound"))
    }
  }

  "Simple Put operation" should "be implemented" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Put")
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("Ok"))
    }
  }

  "Simple Get operation" should "be implemented" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Get")
    SimulationResult += ("keyExists" -> true)
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("Ok"))
      SimulationResult.get[String](s"valuetest$i") should be (Some(s"value$i"))
    }
  }

  "Empty Cas operation" should "return NotFound" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Cas")
    SimulationResult += ("keyExists" -> false)
    SimulationResult += ("keyMatches" -> false)
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("NotFound"))
    }
  }

  "Simple Cas operation" should "be implemented" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Cas")
    SimulationResult += ("keyExists" -> true)
    SimulationResult += ("keyMatches" -> true)
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("Ok"))
      SimulationResult.get[String](s"valuetest$i") should be (Some(s"newValue$i"))
    }
  }

  "Cas operation - key not matching" should "not update" in { // well of course eventually they should be implemented^^
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = SimpleScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> nMessages)
    SimulationResult += ("operation" -> "Cas")
    SimulationResult += ("keyExists" -> true)
    SimulationResult += ("keyMatches" -> false)
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"statustest$i") should be (Some("Ok"))
      SimulationResult.get[String](s"valuetest$i") should be (Some(s"value$i"))
    }
  }
}