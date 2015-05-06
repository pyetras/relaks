package relaks.optimizer

import scala.reflect.runtime.universe._

/**
 * Created by Pietras on 23/04/15.
 */
trait NondetParamTypes {
  abstract class NondetParamType[+T: TypeTag] {
    val typeTag = typeOf[T]

    def view: Seq[T]
  }

  abstract class ContinuousNondetParamType[+T: TypeTag] extends NondetParamType[T] {
    override def view: Seq[T] = throw new NotImplementedError("Continous var space cannot be iterated")
  }

  trait RangeLikeSpace[+T] { this: NondetParamType[T] =>
    def from: T
    def to: T
  }

  case class ContinuousRange(from: Double, to: Double) extends ContinuousNondetParamType[Double] with RangeLikeSpace[Double]

  abstract class DiscreteNondetParamType[+T: TypeTag] extends NondetParamType[T]

  case class DiscreteRange(from: Int, to: Int, step: Int = 1) extends DiscreteNondetParamType[Int] with RangeLikeSpace[Int] {
    val range = new Range(from, to, step)

    override def view: Seq[Int] = range.view
  }

  case class ChooseOneOf[T: TypeTag](choices: Seq[T]) extends DiscreteNondetParamType[T] {
    override def view: Seq[T] = choices
  }

}
