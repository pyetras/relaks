package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.Rep
import relaks.lang.impl.TableImpl
import shapeless.ops.hlist.Mapper
import shapeless.{HNil, ::, Poly1, HList}
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
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

    implicit val tableTranslation = defineTranslation[UntypedTable, TableImpl]
    implicit def translate[T, X](implicit translation: HasTranslation[T, X]) = at[T](x => null.asInstanceOf[X] : X)
  }

  class CallWord[H <: HList, T, Arg <: HList](f: H => Rep[T], typ: TType)(implicit mapper: Mapper.Aux[translate.type, H, Arg]) {
    def apply[AC <: Arg](args: Rep[Tup[AC]]): Rep[T] = new Rep[T] {
      override val tree: Expression = ApplyNative(f, args.tree)(typ)
    }
  }

  private[dsl] trait Converter[R] {
    type Out
    val typ: TType
    def apply(r: R): Rep[Out]
  }

  private[dsl] trait UnliftedConverter[R] extends Converter[R]

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

    implicit def conv2[R](implicit atyp: ArgType[R]): Aux2[R, R] =
      new UnliftedConverter[R] {
        override type Out = R
        override val typ: TType = atyp
        override def apply(r: R): Rep[Out] = r.asRep
      }
  }

  def to[F, H <: HList, R, Arg <: HList](f: F)(implicit fnToProduct: FnToProduct.Aux[F, H => R], converter: Converter[R], mapper: Mapper.Aux[translate.type, H, Arg]) =
    new CallWord((x: H) => converter.apply(f.toProduct(x)), converter.typ)
}
