package fwb.backends.tester

import shapeless._
import syntax.singleton._


/**
 * Created by Pietras on 28/03/15.
 */
object Algorithms {
  case class train(target: List[Boolean]) {
    def apply() = ('ret ->> new Model(target)) :: HNil
  }

  case class Model(labels: List[Boolean]) {
    lazy val labelsfq = labels.foldLeft(0)((acc, v) => if (v) acc + 1 else acc).toDouble / labels.length
    def predict(dataset: List[Double]): List[Boolean] = {
      List.fill(dataset.length)(labelsfq > 0.5)
    }
  }
}
