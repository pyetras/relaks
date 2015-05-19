package relaks.lang.dsl

import AST._
import AST.syntax._
import shapeless._
import shapeless.ops.hlist.{Reverse, Mapper}
import shapeless.ops.product._

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.existentials
import scala.language.reflectiveCalls
/**
 * Created by Pietras on 19/05/15.
 */
//trait Representable[T] {
//  type Out
//
//  def represent(value: T): Out
//
//  def toTree(value: T): Tree
//}
//
//object Representable extends  {
//  type Aux[T, O] = Representable[T] { type Out = O }
//
//  implicit def fromTuple[P <: Product, H <: HList, R <: HList, Mapped <: HList, LU](implicit ev: Generic.Aux[P, H],
//                                           ul: UnliftType.Aux[H, Rep, R],
//                                           evlu: TupleLU[R, LU],
//                                           typC: TupTypeConstructor[R, LU],
//                                           tev: IsTuple[P],
//                                           mapper: Mapper.Aux[asRep.type, H, Mapped],
//                                           traversable: ToTraversable.Aux[Mapped, List, Rep[LU]]): Aux[P, R] = {
//    new Representable[P] {
//      override type Out = R
//
//      override def toTree(value: P): Rep[Out] = {
//        val hlist = ev.to(value)
//        val replist = hlist.map(asRep).toList[Rep[LU]] // <: List[Rep[Any]]
//        val typ = typC(replist.map(_.getTpe))
//        new Rep[Tup[R]] {
//          override val tree: Expression = ProductConstructor(replist.map(_.tree))(typ)
//        }
//
//      }
//
//      override def represent(value: P): Out = ???
//    }
//  }
//
//}

trait UnliftType[-L <: HList, M[_]] {
  type Out <: HList
}
object UnliftType {
  def apply[L <: HList, M[_]](implicit unlift: UnliftType[L, M]) = unlift
  type Aux[L <: HList, M[_], R <: HList] = UnliftType[L, M] { type Out = R }

  implicit def unliftType[H, L <: HList, R <: HList, M[_]]
  (implicit ev: Aux[L, M, R]) :
  Aux[M[H] :: L, M, H :: R] =
    new UnliftType[M[H] :: L, M] { type Out = H :: R }

  implicit def unliftNoType[H, L <: HList, R <: HList, M[_]]
  (implicit ev: Aux[L, M, R], e: H =:!= M[T] forSome { type T }) :
  Aux[H :: L, M, H :: R] =
    new UnliftType[H :: L, M] { type Out = H :: R }

  implicit def unliftNil[M[_]]: Aux[HNil, M, HNil] =
    new UnliftType[HNil, M] { type Out = HNil }
}

trait LiftType[-L <: HList, M[_]] {
  type Out <: HList
}

object LiftType {
  type Aux[L <: HList, M[_], R <: HList] = LiftType[L, M] { type Out = R }
  def apply[L <: HList, M[_]](implicit lift: LiftType[L, M]) = lift

  implicit def liftNil[M[_]]: Aux[HNil, M, HNil] =
    new LiftType[HNil, M] { type Out = HNil }

  implicit def lift[H, L <: HList, R <: HList, M[_]](implicit ev: Aux[L, M, R]): Aux[H :: L, M, M[H] :: R] =
    new LiftType[H :: L, M] {
      override type Out = M[H] :: R
    }

}
trait FillNat[N] extends Serializable {
  type Out <: HList
  def apply(): Out
}

object FillNat {
  def apply[N, L <: HList](implicit fill: FillNat.Aux[N, L], rev: Reverse[L]) = fill

  type Aux[N, Out0] = FillNat[N] { type Out = Out0 }

  implicit def fill1Zero: Aux[Nat._0, Nat._0 :: HNil] =
    new FillNat[Nat._0] {
      type Out = Nat._0 :: HNil
      def apply() = Nat._0 :: HNil
    }

  implicit def fill1Succ[N <: Nat]
  (implicit prev: FillNat[N]): Aux[Succ[N], Succ[N] :: prev.Out] =
    new FillNat[Succ[N]] {
      type Out = Succ[N] :: prev.Out
      def apply() = Succ[N] :: prev()
    }
}
