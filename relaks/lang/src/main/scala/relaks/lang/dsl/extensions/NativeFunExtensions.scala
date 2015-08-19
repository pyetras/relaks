package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.Rep
import relaks.lang.impl.{TypedTableImpl, UntypedTableImpl, Row, TableImpl}
import shapeless.ops.hlist.Mapper
import shapeless.{HNil, ::, Poly1, HList}
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.syntax.std.function._
import scala.language.implicitConversions
/**
 * Created by Pietras on 23/06/15.
 */
trait NativeFunExtensions extends ASTSyntax with AnyExtensions {

  object translate extends Poly1 {
    class HasTranslation[T, X]
    class HasIdentityTranslation[T] extends HasTranslation[T, T]
    implicit def idTranslation[T]: HasIdentityTranslation[T] = new HasIdentityTranslation[T]

    def defineTranslation[B, A] = new Case[A] {
      override type Result = B
      override val value: (::[A, HNil]) => Result = x => null.asInstanceOf[B]
    }

    implicit val tableTranslation = defineTranslation[Table, TableImpl]
    implicit val untypedTableTranslation = defineTranslation[UntypedTable, UntypedTableImpl]
    implicit def typedTableTranslation[H <: HList] = defineTranslation[TypedTable[Tup[H]], TypedTableImpl[H]]
    implicit val rowTranslation = defineTranslation[Tup[_], Row]
    implicit def translate[T, X](implicit translation: HasTranslation[T, X]) = at[T](x => null.asInstanceOf[X] : X)
  }

  class CallWord[T, Arg <: HList](f: Any, typ: TType) {
    def apply[AC <: Arg](args: Rep[Tup[AC]]): Rep[T] = new Rep[T] {
      override val tree: Expression = ApplyNative(f, args.tree)(typ)
    }
  }

  trait Converter[R] {
    type Out
    val typ: TType
    def apply(r: R): Rep[Out]
  }

  trait UnliftedConverter[R] extends Converter[R]

  object Converter {
    type Aux[R, T] = Converter[R] { type Out = T }
    type Aux2[R, T] = UnliftedConverter[R] { type Out = T }
    def apply[R](implicit converter: Converter[R]): Aux[R, converter.Out] = converter

    implicit def conv1[R, T](implicit fn: R => Rep[T], argType: ArgType[T]): Aux[R, T] =
      new Converter[R] {
        type Out = T
        override val typ: TType = argType
        override def apply(r: R): Rep[T] = fn(r)
      }

    implicit def conv2[R: NotLifted](implicit atyp: ArgType[R]): Aux[R, R] =
      new Converter[R] {
        override type Out = R
        override val typ: TType = atyp
        override def apply(r: R): Rep[Out] = r.asRep
      }
  }

  trait Productized[H <: HList, R] {
    def apply(h: H): R
  }

  implicit def productize[F, H <: HList, R](f: F)(implicit fnToProduct: FnToProduct.Aux[F, H => R]) = new Productized[H, R] {
    override def apply(h: H): R = fnToProduct(f)(h)
  }

  //seems that the compiler has problem inferring R: NotLifted, thus the productized object (one less jump)
  def to[F, H <: HList, R, Arg <: HList, T](f: Productized[H, R])(implicit
                                                  converter: Converter.Aux[R, T], fnFromProduct: FnFromProduct[H => Expression],
                                                  mapper: Mapper.Aux[translate.type, H, Arg]) =
    new CallWord[T, Arg](fnFromProduct((x: H) => converter.apply(f(x)).tree), converter.typ)

  def to2[F, H <: HList, R: NotLifted, Arg <: HList, T](f: F)(implicit fnToProduct: FnToProduct.Aux[F, H => R]) = null

}
