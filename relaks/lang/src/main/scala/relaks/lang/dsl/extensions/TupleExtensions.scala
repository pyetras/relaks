package relaks.lang.dsl.extensions

import relaks.lang.dsl._
import AST._
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat.ToInt
import shapeless.syntax.std.tuple._
import relaks.lang.dsl.AST.syntax._


import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.existentials
import scala.language.reflectiveCalls


/**
 * Created by Pietras on 16/04/15.
 */

trait TupleExtensions extends Symbols {

  class TupleOperations[B1 <: HList](val arg1: Rep[Tup[B1]]) {
    val luType = arg1.getTpe.unlift.asInstanceOf[TupType[B1]].lowerBound

    def apply(i: Nat)(implicit toInt: ToInt[i.N], at: At[B1, i.N]) : Rep[at.Out] = arg1.tree match {
      case Expr(ProductConstructor(seq)) => new Rep[at.Out] {
        override val tree: AST.Expression = seq(toInt())
      }
    }

    def at[LUB](i: Rep[Int])(implicit ev: TupleLU[B1, LUB]): Rep[LUB] = new Rep[LUB] { //FIXME jakos?
      override val tree: Expression = Apply(Stdlib.at, List(arg1.tree, i.tree))(luType)
    }
  }

  implicit def tupleToTupleOps[H <: HList](p: Rep[Tup[H]]): TupleOperations[H] = new TupleOperations[H](p)

  /**
   * this will lift any type T to a Rep[T]
    */
  private object asRep extends Poly1 {
    implicit def lifted[T: ArgType] = at[Rep[T]](x => x)
    implicit def unlifted[T: ArgType](implicit c: T => Rep[T]) = at[T](x => c(x))
  }

  implicit def tupleToRep[P <: Product, H <: HList, R <: HList, LU, Mapped <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                ul: UnliftType.Aux[H, Rep, R],
                                                                evlu: TupleLU[R, LU],
                                                                typC: TupTypeConstructor[R, LU],
                                                                tev: IsTuple[P],
                                                                mapper: Mapper.Aux[asRep.type, H, Mapped],
                                                                traversable: ToTraversable.Aux[Mapped, List, Rep[LU]]): Rep[Tup[R]] = {
    val hlist = ev.to(p)
    val replist = hlist.map(asRep).toList[Rep[LU]] // <: List[Rep[Any]]
    val typ = typC(replist.map(_.getTpe))
    new Rep[Tup[R]] {
      override val tree: Expression = ProductConstructor(replist.map(_.tree))(typ)
    }
  }
}

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
