package fwb.dsl.extensions

import fwb.dsl._
import AST._
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat.ToInt
import shapeless.syntax.std.tuple._
import fwb.dsl.AST.syntax._


import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.existentials
import scala.language.reflectiveCalls


/**
 * Created by Pietras on 16/04/15.
 */

trait ProductExtensions extends Symbols {
  trait AtMagnet[B1 <: HList, LU] {
    def apply(tree: Expression, or: SuperPosMapper[Prod[B1, LU], Prod[B1, LU], LU], typ: ArgType[LU]): Rep[LU]
  }

  implicit def natAsMagnet[B1 <: HList](i: Nat)(implicit at: At[B1, i.N], ti: ToInt[i.N]) : AtMagnet[B1, at.Out] = new AtMagnet[B1, at.Out] {
    override def apply(tree: AST.Expression, or: SuperPosMapper[AST.Prod[B1, at.Out], AST.Prod[B1, at.Out], at.Out], typ: ArgType[at.Out]): Rep[at.Out] = tree match {
      case Expr(ProductConstructor(seq)) => new Rep[at.Out] {
        override val tree: AST.Expression = seq(ti.apply())
      }
    }
  }

  implicit def repiAsMagnet[B1 <: HList, LU](i: Rep[Int]): AtMagnet[B1, LU] = new AtMagnet[B1, LU] {
    override def apply(tree: AST.Expression, or: SuperPosMapper[Prod[B1, LU], Prod[B1, LU], LU], typ: ArgType[LU]): Rep[LU] = new Rep[LU] {
      override val tree: Expression = Apply(Stdlib.at, List(tree, i.tree))(typ)
    }
  }

  class ProductOperations[B1 <: HList, LU](val arg1: Rep[Prod[B1, LU]]) extends Operations[Prod[B1, LU]] {
    implicit def luType = arg1.getTpe.asInstanceOf[ProdType[B1, LU]].luType

    def apply[T](t: T)(implicit magnet: T => AtMagnet[B1, LU]) = t(tree, op.to[LU], luType)
  }

  implicit def productToProductOps[H <: HList, LU](p: Rep[Prod[H, LU]])(implicit typ: ProdType[H, LU]) = new ProductOperations[H, LU](p)

  private[this] object asRep extends Poly1 {
    implicit def lifted[T: ArgType] = at[Rep[T]](x => x)
    implicit def unlifted[T: ArgType](implicit c: T => Rep[T]) = at[T](x => c(x))
  }

  implicit def productToRep[P <: Product, H <: HList, R <: HList, LU, Mapped <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                ul: UnliftType.Aux[H, Rep, R],
                                                                typ: ProdType[R, LU],
                                                                tev: IsTuple[P],
                                                                mapper: Mapper.Aux[asRep.type, H, Mapped],
                                                                traversable: ToTraversable.Aux[Mapped, List, Rep[LU]]): Rep[Prod[R, LU]] = {
    val hlist = ev.to(p)
    val replist = hlist.map(asRep).toList[Rep[LU]] // <: List[Rep[Any]]
    new Rep[Prod[R, LU]] {
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
