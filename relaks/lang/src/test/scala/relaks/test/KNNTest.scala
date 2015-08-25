package relaks.test

import breeze.linalg._
import breeze.linalg
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
import scala.collection.JavaConverters._

/**
 * Created by Pietras on 09.08.15.
 */
class KNNTest extends FunSpec with Matchers with Inside {
  describe("knn experiment") {
    it("should work", Tag("relaks.Integration")) {
      object Program extends DSLOptimizerInterpreter with DrillInterpreter with ListComprehensionInterpreter with ComprehensionPrinters
      import Program._

      case class FeatureVec(features: Vector[Double], klass: Int)

      def normDistance(n: Double) = new DistanceFunction[FeatureVec] {
        override def calculate(data1: FeatureVec, data2: FeatureVec): Double = norm(data1.features - data2.features, n)
      }

      def hammingDistance = new DistanceFunction[FeatureVec] {
        override def calculate(data1: FeatureVec, data2: FeatureVec): Double = {
          if (data1.features.length != data2.features.length) throw new Exception("cannot compute hamming distance for vectors of different length")
          var distance = 0
          zipValues(data1.features, data2.features).foreach {(x1, x2) => if (x1 != x2) distance+=1}
          distance
        }
      }

      def makeTree(dist: DistanceFunction[FeatureVec], train: TableImpl) = {
        val tree = new MTree(dist, null)
        train.toIterator.foreach(row => tree.add(row(0).asInstanceOf[FeatureVec]))
        tree
      }

      def knn(k: Int, tree: MTree[FeatureVec], test: TableImpl) = {
        val confMatrix = DenseMatrix.zeros[Double](11, 11)
        test.toIterator.foreach { row =>
          val fv = row(0).asInstanceOf[FeatureVec]
          val query = tree.getNearestByLimit(fv, k)
          val votes = query.iterator().asScala
            .foldLeft(Map.empty[Int, Int].withDefaultValue(0))((acc, vote) => acc + (vote.data.klass -> (acc(vote.data.klass) + 1)))

          val voted = votes.maxBy({ case (klass, count) => count })._1
          confMatrix(fv.klass, voted) += 1.0
        }

        confMatrix
      }


      val distFn = choose from List(normDistance(1).asRep, normDistance(2).asRep, hammingDistance.asRep)
      val k = choose between 1 and 5
      val ds = load("/Users/Pietras/Downloads/train-vowels.csv")

      def dataSet(filter: Row[Int] => Rep[Boolean]) = ds.filter(Tuple1("columns[11]".::[Int]))(filter).map({(row: Rep[UntypedTup]) =>
        val vec = row.liftMap(rowi => FeatureVec(rowi[Double](0 to 9), rowi.get[Int](10)))
        vec as 'features
      })

      val experiment = optimize((distFn, k)) map { case Tup(dist, k) =>
        val avgF1 = List(1, 2, 3, 4).asTable.map { case Tup(Tuple1(fold)) =>

          val test = dataSet(row => row(0) === fold)
          val train = dataSet(row => row(0) !== fold)

          val tree = (makeTree _).pure.apply((dist, train))

          val confMatrix: Rep[DenseMatrix[Double]] = (knn _).pure.apply((k, tree, test))

          confMatrix.liftMap(mx => -f1_accuracy(mx)._1) as 'result
        }.avg

        (k as 'k, dist as 'distFn, avgF1 as 'result)
      } by 'result

      store(experiment)
      dump()
    }
  }
}
