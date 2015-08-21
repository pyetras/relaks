package relaks.data

import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Mapper, Length}
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.record.Selector
import scala.language.dynamics
import scala.language.implicitConversions

/**
 * Created by Pietras on 02/05/15.
 */
trait LabelledTuples extends LabelledImplicitConversions {
  class LabelledTuple[R <: HList](tuple: R) extends Dynamic with Product {

    override def productElement(n: Int): Any = tuple.productElement(n)

    override def productArity: Int = tuple.productArity

    override def canEqual(that: Any): Boolean = tuple.canEqual(that)

    def apply(name: Witness)(implicit sel: Selector[R, name.T]) = sel(tuple)
    def selectDynamic(name: Witness)(implicit sel: Selector[R, name.T]) = sel(tuple)

    def selector[K](implicit sel: Selector[R, K]) = LabelledTuple.selector[R, K]

    val asHList = tuple

    override def toString: String = tuple.toString
  }

  sealed trait LabelledTupleSelector[R <: HList, O]{
    def apply(r: LabelledTuple[R]): O
  }

  object LabelledTuple {
    def selector[R <: HList, K](implicit sel: Selector[R, K]) = new LabelledTupleSelector[R, sel.Out] {
      override def apply(r: LabelledTuple[R]): sel.Out = sel(r.asHList)
    }
  }

  object isField extends Poly1 {
    implicit def aField[K, V] = at[FieldType[K, V]](x => ???)
  }

  implicit def addAs[T](t: T): AnyAs[T] = new AnyAs(t)
}

class AnyAs[T](val t: T) extends AnyVal {
  def as(name: Witness): FieldType[name.T, T] = field[name.T](t)
}

trait LabelledImplicitConversions { this: LabelledTuples =>
  implicit def fromTupleWithLabels[P <: Product, L <: HList](tup: P)(implicit //ev1: IsTuple[P], TODO: ?
                                                                     toHlist: Generic.Aux[P, L],
                                                                     evR: Mapper[isField.type, L]): LabelledTuple[L] =
    new LabelledTuple(toHlist.to(tup))

  implicit def fromSingleField[K, V](field: FieldType[K, V]): LabelledTuple[FieldType[K, V] :: HNil] = new LabelledTuple(field :: HNil)

}