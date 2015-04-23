package relaks.optimizer

import scala.reflect.runtime.universe._

/**
 * Created by Pietras on 23/04/15.
 */
trait VarSpaceTypes {
  abstract class VarSpace[+T: TypeTag] {
    val typeTag = typeOf[T]

    def view: Seq[T]
  }

  abstract class ContinuousVarSpace[+T: TypeTag] extends VarSpace[T] {
    override def view: Seq[T] = throw new NotImplementedError("Continous var space cannot be iterated")
  }

  trait RangeLikeSpace[+T] { this: VarSpace[T] =>
    def from: T
    def to: T
  }

  case class ContinuousRange(from: Double, to: Double) extends ContinuousVarSpace[Double] with RangeLikeSpace[Double]

  abstract class DiscreteVarSpace[+T: TypeTag] extends VarSpace[T]

  case class DiscreteRange(from: Int, to: Int, step: Int = 1) extends DiscreteVarSpace[Int] with RangeLikeSpace[Int] {
    val range = new Range(from, to, step)

    override def view: Seq[Int] = range.view
  }

  case class ChooseOneOf[T: TypeTag](choices: Seq[T]) extends DiscreteVarSpace[T] {
    override def view: Seq[T] = choices
  }

}
