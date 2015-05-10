package relaks.optimizer

import org.scalatest._

/**
 * Created by Pietras on 07/05/15.
 */
class ExperimentTest extends FunSpec with Matchers {
//  describe("Experiment runner") {
//    it("should optimize branin using spearmint") {
//      def branin(x: Double, y: Double) = {
//        val sq: (Double => Double) = math.pow(_, 2)
//        sq(y - (5.1/(4*sq(math.Pi)))*sq(x) + (5/math.Pi)*x - 6) + 10*(1-(1./(8*math.Pi)))*math.cos(x) + 10
//      }
//      object TestExperiment extends Experiments with SpearmintOptimizer
//      import TestExperiment._
//      val varSpace = ParamProvider(
//        "x" -> ContinuousRange(0, 15),
//        "y" -> ContinuousRange(-5, 10))
//
//      val results = Experiment search varSpace minimize "result" in {
//        val result = branin(varSpace.x.as[Double], varSpace.y.as[Double])
//        result as "result"
//      }
//
//      val l = results.take(100).runLog.run
//      l foreach println
//    }
//  }

}
