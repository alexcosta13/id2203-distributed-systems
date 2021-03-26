package se.kth.id2203.simulation

import org.scalatest._

import se.sics.kompics.sl.simulator._
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.result.SimulationResultSingleton

class LinTest extends FlatSpec with Matchers {

  "R/W operations" should "be linearizable" in {
    val seed = 123L
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = LinScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("messages" -> 10)
    SimulationResult += ("operation" -> "LinTest")
    simpleBootScenario.simulate(classOf[LauncherComp])

    SimulationResult.get[String]("linearizable") should be (Some("true"))
  }
}