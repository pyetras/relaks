package fwb.dsl.extensions

import fwb.dsl._
import AST._
import shapeless._
import shapeless.syntax.std.tuple._
import fwb.dsl.AST.syntax._


import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.existentials

/**
 * Created by Pietras on 16/04/15.
 */
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
  (implicit ev: Aux[L, M, R], e: H =:!= Rep[T] forSome { type T }) :
  Aux[H :: L, M, H :: R] =
    new UnliftType[H :: L, M] { type Out = H :: R }

  implicit def unliftNil[H, M[_]]
  (implicit e: H =:!= Rep[T] forSome { type T }) :
  Aux[H :: HNil, M, H :: HNil] =
    new UnliftType[H :: HNil, M] { type Out = H :: HNil }

  implicit def unliftNilType[H, M[_]] :
  Aux[M[H] :: HNil, M, H :: HNil] =
    new UnliftType[M[H] :: HNil, M] { type Out = H :: HNil }
}

trait ProductExtensions {
  implicit def tupleToRep[P <: Product, H <: HList, R <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                ul: UnliftType.Aux[H, Rep, R],
                                                                typ: ProdType[R],
                                                                tev: IsTuple[P]) : Rep[ProdType[R]] =
    new Rep[ProdType[R]] {
      override def tree: TTree = Literal(true) //FIXME
    }
}
