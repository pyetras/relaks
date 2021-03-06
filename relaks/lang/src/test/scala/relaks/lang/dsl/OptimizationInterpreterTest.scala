package relaks.lang.dsl

import org.scalatest.{Tag, Matchers, Inside, FunSpec}
import relaks.lang.ast._
import relaks.optimizer.SpearmintOptimizer
import shapeless.{HNil, ::}

import scalaz.stream.process1

/**
 * Created by Pietras on 12/07/15.
 */
class OptimizationInterpreterTest extends FunSpec with Matchers with Inside {
  describe("interpreter") {
    it("should interpret a simple expression") {
      object Program extends DSLOptimizerInterpreter
      import Program._
      val b = choose between 1 and 3
//      val a = choose between 5 and 10
      val r = (optimize (Tuple1(b)) map { row =>
        Tuple1(row(0))
      }) by 'x0


      val Some((rows, error)) = Program.run(r.tree)
      rows should have length 3
      error shouldBe empty
    }

    it("should print a stored table") {
      object Program extends DSLOptimizerInterpreter
      import Program._

      val b = choose between 1 and 10
      val c: Rep[UnfinishedGenTable[Tup[Int::Int::HNil]]] = optimize(Tuple1(b)) map { row =>
        (1 as 'var1, row(0) as 'var2)
      }
      val r = c filter { row =>
        row(1) > 1
      } by 'var2 limit 3 orderBy ('var1, 'var2)
      store(r)

      val Some((rows, error)) = Program.run(r.tree)
      rows should have length 3
      error shouldBe empty
//      Program.show(rows, error)
      Program.dump()
    }

    it("should optimize branin", Tag("relaks.Integration")) {
      object Program extends DSLOptimizerInterpreter(SpearmintOptimizer) {
        val x = choose between 0.0 and 15.0
        val y = choose between -5.0 and 10.0

        val result = optimize (x, y) map { case Tup(x, y) =>
          val res = to (branin _) apply (x, y)
          res as 'result
        } by 'result

        store(result)
      }

      Program.dump()
    }

//    it("should execute an optimization scenario") {
//      object Program extends DSLOptimizerInterpreter
//      import Program._
//
//      def trainSVM(a: Any, b: Any, c: Any, d: Any) = 0.0
//      def trainkNN(a: Any, b: Any, c: Any) = 0.0
//      def avg(a: Any): Rep[Double] = 0.0
//      def ifelse(cond: => Rep[Boolean])(f1: => Any)(f2: => Any) = 0.0
//
//      val folds = List(1, 2, 3, 4, 5).asTable()
//      val data = load("train.csv")
//      val C = choose between 1 and 10
//      val width = choose between 1 and 10
//      val model = choose from List("SVM", "knn")
//      val k = choose between 1 and 5
//
//      val experiment: Rep[UntypedTable] = folds map { foldsRow =>
//        val trainIx = data('fold.::[Int]) filter { sample =>
//          val Tup(fold) = sample
//          fold !== foldsRow(0)
//        }
//
//        val testIx = data('result.::[Int], 'fold.::[Int]) filter { sample =>
//          val Tup(fold) = sample
//          fold === foldsRow(0)
//        }
//
//        val fitness = ifelse(model === "SVM") {
//          trainSVM(data, testIx, trainIx, C, width)
//        } {
//          trainkNN(data, testIx, trainIx, k)
//        }
//
//        avg(fitness) as 'fitnessAvg
//      }
//
//      val result = optimize(Tuple1(experiment)) flatMap { table =>
//        table(0)
//      } orderBy 'fitnessAvg
//
//      store(result)
//
//      Program.dump()
//    }

  }

}
