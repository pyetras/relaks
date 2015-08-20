package relaks.lang.dsl.utils

import relaks.lang.ast.{TypedField, Field, ArgType, TType}
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
    def apply(l: L): Vector[Field] @@ Out
  }

  object TypedSymbolSchema {
    type Aux[L <: HList, R <: HList] = TypedSymbolSchema[L] { type Out = R }
    def apply[L <: HList](implicit schema: TypedSymbolSchema[L]): Aux[L, schema.Out] = schema

    object buildSchema extends Poly2 {
      implicit def f[E, S <: HList] = at[TypedField[E], Vector[Field] @@ S] {
        case (field, taggedSchema) => Tag.of[E :: S](field +: Tag.unwrap(taggedSchema))
      }
    }

    implicit def typedSymbolSchema[L <: HList, S <: HList]
      (implicit folder: RightFolder.Aux[L, Vector[Field] @@ HNil, buildSchema.type, Vector[Field] @@ S]): Aux[L, S] = {
      new TypedSymbolSchema[L] {
        override type Out = S
        override def apply(l: L): Vector[Field] @@ S = l.foldRight(Tag.of[HNil](Vector.empty[Field]))(buildSchema)
      }
    }
  }

  implicit class TypedSymbol(sym: Symbol) {
    def is[T: ArgType] = new TypedField[T](sym, implicitly[ArgType[T]])
    def ::[T: ArgType] = is[T]
  }

  implicit class TypedString(str: String) {
    def ::[T: ArgType] = Symbol(str).is[T]
  }
}
