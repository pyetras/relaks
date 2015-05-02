package relaks.data

import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.Mapper
import shapeless._
import shapeless.ops.record.Selector
import scala.language.dynamics
import scala.language.implicitConversions

/**
 * Created by Pietras on 02/05/15.
 */
trait LabelledTuples {
  class LabelledTuple[R <: HList](tuple: R) extends Dynamic {
    def apply(name: Witness)(implicit sel: Selector[R, name.T]) = sel(tuple)
    def selectDynamic(name: Witness)(implicit sel: Selector[R, name.T]) = sel(tuple)

    def selector[K](implicit sel: Selector[R, K]) = LabelledTuple.selector[R, K]

    def toHList = tuple
  }

  sealed trait LabelledTupleSelector[R <: HList, O]{
    def apply(r: LabelledTuple[R]): O
  }

  object LabelledTuple {
    def selector[R <: HList, K](implicit sel: Selector[R, K]) = new LabelledTupleSelector[R, sel.Out] {
      override def apply(r: LabelledTuple[R]): sel.Out = sel(r.toHList)
    }
  }

  private[this] object isRecord extends Poly1 {
    implicit def aField[K, V] = at[FieldType[K, V]](x => ???)
  }

  implicit def fromTupleWithLabels[P <: Product, L <: HList](tup: P)(implicit //ev1: IsTuple[P], TODO: ?
                                                                     toHlist: Generic.Aux[P, L],
                                                                     evR: Mapper[isRecord.type, L]): LabelledTuple[L] =
    new LabelledTuple(toHlist.to(tup))

  implicit def addAs[T](t: T): AnyAs[T] = new AnyAs(t)
}

class AnyAs[T](val t: T) extends AnyVal {
  def as(name: Witness): FieldType[name.T, T] = field[name.T](t)
}

