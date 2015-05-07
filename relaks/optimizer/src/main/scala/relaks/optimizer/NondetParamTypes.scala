package relaks.optimizer

import scala.reflect.runtime.universe._
import scalaz._
import Scalaz._

/**
 * Created by Pietras on 23/04/15.
 */
trait NondetParamTypes {
  abstract sealed class NondetParamType[+T: TypeTag] {
    val typeTag = typeOf[T]

    def view: Seq[T]
  }

  sealed abstract class ContinuousNondetParamType[+T: TypeTag] extends NondetParamType[T] {
    override def view: Seq[T] = throw new NotImplementedError("Continous var space cannot be iterated")
  }

  sealed trait RangeLikeSpace[+T] { this: NondetParamType[T] =>
    def from: T
    def to: T
  }

  //Any -> to suppress: The outer reference in this type test cannot be checked at run time.
  object RangeLikeSpace {
    def unapply(rangeLikeSpace: RangeLikeSpace[_]): Option[(Any, Any)] = (rangeLikeSpace.from, rangeLikeSpace.to).some
  }

  sealed case class ContinuousRange(from: Double, to: Double) extends ContinuousNondetParamType[Double] with RangeLikeSpace[Double]

  sealed abstract class DiscreteNondetParamType[+T: TypeTag] extends NondetParamType[T]

  sealed case class DiscreteRange(from: Int, to: Int, step: Int = 1) extends DiscreteNondetParamType[Int] with RangeLikeSpace[Int] {
    val range = new Range(from, to, step)

    override def view: Seq[Int] = range.view
  }

  sealed case class ChooseOneOf[T: TypeTag](choices: Seq[T]) extends DiscreteNondetParamType[T] {
    override def view: Seq[T] = choices
  }

}
