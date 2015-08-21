package relaks.test

import breeze.linalg.{Vector, norm, zipValues}
import breeze.math.Ring
import breeze.numerics._
import org.scalatest.{Tag, FunSpec, Inside, Matchers}
import relaks.lang.ast.{Tup, UntypedTup}
import relaks.lang.dsl.extensions.ast.logical.{Comprehension, ComprehensionPrinters}
import relaks.lang.dsl.{DSLOptimizerInterpreter, Rep}
import relaks.lang.impl.TableImpl
import relaks.lang.phases.interpreter.{DrillInterpreter, ListComprehensionInterpreter}
import relaks.lib.mtree.{DistanceFunction, MTree}
import relaks.optimizer.SpearmintOptimizer
import relaks.lang.impl
import shapeless._

/**
 * Created by Pietras on 09.08.15.
 */
class KNNTest extends FunSpec with Matchers with Inside {
  describe("knn experiment") {
    it("should work", Tag("relaks.Integration")) {
      object Program extends DSLOptimizerInterpreter(SpearmintOptimizer) with DrillInterpreter with ListComprehensionInterpreter with ComprehensionPrinters
      import Program._

      def euclidDistance[T](implicit ring: Ring[T], sq: sqrt.Impl[T, Double]) = new DistanceFunction[Vector[T]] {
        override def calculate(data1: Vector[T], data2: Vector[T]): Double = {
          val diff = data1 - data2
          sqrt(diff.dot(diff))
        }
      }


      def manhattanDistance[T : Numeric](implicit ring: Ring[T], n: norm.Impl2[Vector[T], Double, Double]) = new DistanceFunction[Vector[T]] {
        override def calculate(data1: Vector[T], data2: Vector[T]): Double = norm(data1 - data2, 1.0)
      }

      def hammingDistance = new DistanceFunction[Vector[Int]] {
        override def calculate(data1: Vector[Int], data2: Vector[Int]): Double = {
          if (data1.length != data2.length) throw new Exception("cannot compute hamming distance for vectors of different length")
          var distance = 0
          zipValues(data1, data2).foreach {(x1, x2) => if (x1 != x2) distance+=1}
          distance
        }
      }

      def makeTree(dist: DistanceFunction[Vector[Double]], train: TableImpl) = {
        val tree = new MTree(dist, null)
        tree
      }

      def knn(k: Int, tree: MTree[Vector[Double]], test: TableImpl) = {

      }


      val distFn = choose from List(euclidDistance[Double].asRep, manhattanDistance[Double].asRep)
      val ds = load("/Users/Pietras/Downloads/train-vowels.csv")/*("columns[0]".::[Double],
//        "columns[1]".::[Double], "columns[2]".::[Double], "columns[3]".::[Double], "columns[4]".::[Double], "columns[5]".::[Double],
//        "columns[6]".::[Double], "columns[7]".::[Double], "columns[8]".::[Double], "columns[9]".::[Double],
//        "columns[10]".::[Int], "columns[11]".::[Int])*/
      val res = ds.filter(Tuple1("columns[11]".::[Int]))( (row:Row[Int]) =>
        row(0) > 1
      ).map((row: Rep[UntypedTup]) =>
        row.liftMap((rowi:impl.UntypedRow) => {
          (rowi[Double](0 to 9).asRep as 'fetaures, rowi.get[Int](10) as 'class)
//          (Vector(1.0, 2.0).asRep as 'fetures, 1 as 'class)
        })
        )
//      val res: Rep[Tup[Vector[Double] :: Int :: HNil]] = (Vector(1.0, 2.0).asRep as 'fetures, 1 as 'class)

//      ret

//      val i: Rep[Int] = 1
//      val ii = i.liftMap(x => x + 10)

//      val res = ds map { row =>
//        (List(row(0), row(1), row(2), row(3), row(4), row(5), row(6), row(7), row(8), row(9)) as 'features, row(10) as 'class, row(11) as 'fold)
//      } limit 10

//      val test = load("test.csv")

//      val tree = to (makeTree _) apply (euclidDistance[Double].asRep, train)
//      val results = to (knn _) apply (5, tree, test)
//
//      println(tree.toString)
      val _/>(c: Comprehension) = buildComprehensions(res.tree).get
      println(ComprehensionPrinter.apply(c))
      store(res limit 10)
      dump()
//      println(evalExpression(ii.tree))
    }
  }
}
