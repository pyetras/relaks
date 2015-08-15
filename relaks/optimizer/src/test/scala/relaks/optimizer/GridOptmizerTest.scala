package relaks.optimizer

/**
 * Created by Pietras on 02/05/15.
 */

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest._


class GridOptmizerTest extends FunSpec with Matchers with Inside {
  describe("Experiment with grid optimizer") {
    it("should return correct number of results") {
      import shapeless._
      import syntax.singleton._

      val cnt = new AtomicInteger(0)

      def advancedMLAlgorithm(x: Int, y: Int) = {
//        println(x)
//        println(y)
        cnt.incrementAndGet()
//        Thread.sleep(10)
        x*x + y
      }

      object TestExperiment extends Experiments with GridOptimizer
      import TestExperiment._

      val varSpace = ParamProvider(
        "x" -> DiscreteRange(0, 50),
        "y" -> ChooseOneOf(List(-1, -2, -3, -4)))

      val results = Experiment search varSpace minimize "result" in {
        val result = advancedMLAlgorithm(varSpace.x.as[Int], varSpace.y.as[Int])
        result as "result"
      }

      results.run.run
//      println(cnt.get())
      cnt.get() should equal(204)
    }
  }
}
