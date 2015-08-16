package relaks.lang.dsl

import breeze.linalg.{norm, sum, zipValues, Vector}
import breeze.math.Ring
import breeze.numerics._
import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast.{Tup, UntypedTable}
import relaks.lang.impl.TableImpl
import relaks.lib.mtree.{MTree, DistanceFunction}
import shapeless.{::, HNil}

/**
 * Created by Pietras on 09.08.15.
 */
class KNNTest extends FunSpec with Matchers with Inside {
  describe("knn experiment") {
    it("should work") {
      object Program extends DSLInterpreter
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
      val train = load("train.csv")
      val test = load("test.csv")

      val tree = to (makeTree _) apply (euclidDistance[Double].asRep, train)
      val results = to (knn _) apply (5, tree, test)

      println(tree.toString)
    }
  }
}
