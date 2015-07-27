package relaks.lang.dsl.utils

import shapeless.{HNil, Poly2, HList, ::}
import shapeless.ops.hlist.{RightFolder, RightReducer}

import scalaz.{@@, Tag}
import scalaz.Scalaz._

/**
 * Created by Pietras on 28.07.15.
 */
trait TypedSymbols {
  trait TypedSymbolSchema[L <: HList] {
    type Out <: HList
    def apply(l: L): Vector[Symbol] @@ Out
  }

  object TypedSymbolSchema {
    type Aux[L <: HList, R <: HList] = TypedSymbolSchema[L] { type Out = R }
    def apply[L <: HList](implicit schema: TypedSymbolSchema[L]): Aux[L, schema.Out] = schema

    object buildSchema extends Poly2 {
      implicit def f[E, S <: HList] = at[Symbol @@ E, Vector[Symbol] @@ S] {
        case (taggedSym, taggedSchema) => Tag.of[E :: S](Tag.unwrap(taggedSym) +: Tag.unwrap(taggedSchema))
      }
//      implicit def f2[S <: HList] = at[Symbol, Vector[Symbol] @@ S] {
//        case (sym, taggedSchema) => Tag.of[Any :: S](sym +: Tag.unwrap(taggedSchema))
//      }
    }

    implicit def typedSymbolSchema[L <: HList, S <: HList](implicit folder: RightFolder.Aux[L, Vector[Symbol] @@ HNil, buildSchema.type, Vector[Symbol] @@ S]): Aux[L, S] = {
      new TypedSymbolSchema[L] {
        override type Out = S
        override def apply(l: L): Vector[Symbol] @@ S = l.foldRight(Tag.of[HNil](Vector.empty[Symbol]))(buildSchema)
      }
    }
  }

  implicit class TypedSymbol(sym: Symbol) {
    def is[T] = Tag.of[T](sym)
  }
}
