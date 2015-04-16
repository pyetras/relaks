package fwb.dsl.ops

import fwb.dsl._
import AST._
import shapeless._
import shapeless.syntax.std.tuple._
import fwb.dsl.AST.syntax._


import scala.language.implicitConversions
import scala.language.existentials

/**
 * Created by Pietras on 16/04/15.
 */
trait UnliftRep[-L <: HList] {
  type Out <: HList
}
object UnliftRep {
  def apply[L <: HList](implicit unlift: UnliftRep[L]) = unlift
  type Aux[L <: HList, R <: HList] = UnliftRep[L] { type Out = R }

  implicit def unliftRep[H, L <: HList, R <: HList]
  (implicit ev: Aux[L, R]) :
  Aux[Rep[ArgType[H]] :: L, H :: R] =
    new UnliftRep[Rep[ArgType[H]] :: L] { type Out = H :: R }

  implicit def unliftNoRep[H, L <: HList, R <: HList]
  (implicit ev: Aux[L, R], e: H =:!= Rep[T] forSome { type T }) :
  Aux[H :: L, H :: R] =
    new UnliftRep[H :: L] { type Out = H :: R }

  implicit def unliftNil[H]
  (implicit e: H =:!= Rep[T] forSome { type T }) :
  Aux[H :: HNil, H :: HNil] =
    new UnliftRep[H :: HNil] { type Out = H :: HNil }

  implicit def unliftNilRep[H] :
  Aux[Rep[ArgType[H]] :: HNil, H :: HNil] =
    new UnliftRep[Rep[ArgType[H]] :: HNil] { type Out = H :: HNil }
}

trait ProductOps {
  implicit def tupleToRep[P <: Product, H <: HList, R <: HList](p: P)(implicit ev: Generic.Aux[P, H], ul: UnliftRep.Aux[H, R], typ: ProdType[R], tev: IsTuple[P]) : Rep[ProdType[R]] =
    new Rep[ProdType[R]] {
      override def tree: TTree = Literal(true)
    }
}
