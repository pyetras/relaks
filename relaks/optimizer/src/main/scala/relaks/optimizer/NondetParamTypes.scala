package relaks.optimizer

import scala.collection.immutable.Range.Inclusive
import scala.reflect.runtime.universe._
import scalaz._
import Scalaz._

/**
 * Created by Pietras on 23/04/15.
 */
abstract sealed class NondetParam[+T: TypeTag] {
  val typeTag = typeOf[T]

  def view: Seq[T]
}

sealed abstract class ContinuousNondetParam[+T: TypeTag] extends NondetParam[T] {
  override def view: Seq[T] = throw new NotImplementedError("Continous var space cannot be iterated")
}

sealed trait RangeLikeSpace[+T] { this: NondetParam[T] =>
  def from: T
  def to: T
}

//Any -> to suppress: The outer reference in this type test cannot be checked at run time.
object RangeLikeSpace {
  def unapply(rangeLikeSpace: RangeLikeSpace[_]): Option[(Any, Any)] = (rangeLikeSpace.from, rangeLikeSpace.to).some
}

sealed case class ContinuousRange(from: Double, to: Double) extends ContinuousNondetParam[Double] with RangeLikeSpace[Double]

sealed abstract class DiscreteNondetParam[+T: TypeTag] extends NondetParam[T]

sealed case class DiscreteRange(from: Int, to: Int, step: Int = 1) extends DiscreteNondetParam[Int] with RangeLikeSpace[Int] {
  val range = new Inclusive(from, to, step)

  override def view: Seq[Int] = range.view
}

sealed case class ChooseOneOf[T: TypeTag](choices: Seq[T]) extends DiscreteNondetParam[T] {
  override def view: Seq[T] = choices
}

