package relaks.optimizer

/**
 * Created by Pietras on 02/05/15.
 */
import org.scalatest._


class GridOptmizerTest extends FunSpec with Matchers with Inside {
  describe("Experiment with grid optimizer") {
    it("should return correct number of results") {
      import shapeless._
      import syntax.singleton._

      def advancedMLAlgorithm(x: Int, y: Int) = {
        println(x)
        println(y)
        x*x + y
      }

      object TestExperiment extends Experiments with GridOptimizer
      import TestExperiment._

      val varSpace = VarProvider(Map(
        "x" -> DiscreteRange(1, 200),
        "y" -> ChooseOneOf(List(-1, -2, -3, -4))))

      val results = Experiment search varSpace minimize "result" in {
        val result = advancedMLAlgorithm(varSpace.x.as[Int], varSpace.y.as[Int])
        Tuple1(result as "result")
      }

      results should have length 100
    }
  }
}
