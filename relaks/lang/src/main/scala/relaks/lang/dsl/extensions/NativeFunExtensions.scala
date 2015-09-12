package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.Rep
import relaks.lang.impl
import relaks.lang.impl.TableProcess
import shapeless.ops.hlist.Mapper
import shapeless._
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.syntax.std.function._
import scala.language.implicitConversions
/**
 * Created by Pietras on 23/06/15.
 */
trait NativeFunExtensions extends ASTSyntax with AnyExtensions {


  trait MapArgs[A <: HList] {
    type Out <: HList
  }
  object MapArgs {
    type Aux[A <: HList, B <: HList] = MapArgs[A] {type Out = B}

    def apply[T <: HList](implicit t: MapArgs[T]): Aux[T, t.Out] = t

    implicit def translateNil: Aux[HNil, HNil] = new MapArgs[HNil] {
      type Out = HNil
    }

    implicit def mapArg[A, B, HA <: HList, HB <: HList](implicit ev: Aux[HA, HB],
                                                           c: Translation[B, A]): Aux[A :: HA, B :: HB] =
      new MapArgs[A :: HA] {
        type Out = B :: HB
      }

    implicit def mapArgId[A, HA <: HList, HB <: HList](implicit ev: Aux[HA, HB], nc: Not[Translation[A, A]]): Aux[A :: HA, A :: HB]
    = new MapArgs[A :: HA] {
      type Out = A :: HB
    }

    class Not[X]

    implicit def not[X, Y]: Not[Translation[X, Y]] = new Not[Translation[X, Y]]

    implicit def notNot[X, Y](implicit c: Translation[_, Y]): Not[Translation[X, Y]] = new Not[Translation[X, Y]]
    implicit def notNot2[X, Y](implicit c: Translation[X, _]): Not[Translation[X, Y]] = new Not[Translation[X, Y]]

    class Translation[X, Y]

    implicit val tableTranslation = new Translation[Table, impl.Table]
    implicit val untypedTableTranslation = new Translation[UntypedTable, impl.UntypedTable]

    implicit def typedTableTranslation[H <: HList] = new Translation[TypedTable[Tup[H]], impl.TypedTable[H]]

    implicit val rowTranslation = new Translation[Tupl, impl.Row]
    implicit val untypedRowTranslation = new Translation[UntypedTup, impl.UntypedRow]

//    implicit def notUnfinishedGenTranslation = new Translation[Nothing, U]
  }
//  object translate extends Poly1 {
//    class HasTranslation[T, X]
//    class HasIdentityTranslation[T] extends HasTranslation[T, T]
//    implicit def idTranslation[T]: HasIdentityTranslation[T] = new HasIdentityTranslation[T]
//
//    def defineTranslation[B, A] = new Translation[A] {
//      override type Result = B
//      override val value: (::[A, HNil]) => Result = x => null.asInstanceOf[B]
//    }
//
//    implicit val tableTranslation = defineTranslation[Table, TableProcess]
//    implicit val untypedTableTranslation = defineTranslation[UntypedTable, impl.UntypedTable]
//    implicit def typedTableTranslation[H <: HList] = defineTranslation[TypedTable[Tup[H]], impl.TypedTable[H]]
//    implicit val rowTranslation = defineTranslation[Tup[_], Row]
//    implicit def translate[T, X](implicit translation: HasTranslation[T, X]) = at[T](x => null.asInstanceOf[X] : X)
//  }

  class CallWord[T, Arg <: HList](f: Any, typ: TType) {
    def apply[AC <: Arg](args: Rep[Tup[AC]]): Rep[T] = new Rep[T] {
      override val tree: Expression = ApplyNative(f, args.tree)(typ)
    }
  }

  trait Converter[-R] {
    type Out
    val typ: TType
    def apply(r: R): Rep[Out]
  }

  object Converter {
    type Aux[-R, T] = Converter[R] { type Out = T }
    def apply[R](implicit converter: Converter[R]): Aux[R, converter.Out] = converter

    implicit def conv0[T: ArgType]: Aux[Rep[T], T] = new Converter[Rep[T]] {
      override type Out = T
      override def apply(r: Rep[T]): Rep[T] = r
      override val typ: TType = implicitly[ArgType[T]]
    }

//    implicit def conv1[R, T](implicit fn: R => Rep[T], argType: ArgType[T]): Aux[R, T] =
//      new Converter[R] {
//        type Out = T
//        override val typ: TType = argType
//        override def apply(r: R): Rep[T] = fn(r)
//      }

    implicit def conv2[R: ArgType](implicit ev: R <:!< Rep[_]): Aux[R, R] =
      new Converter[R] {
        override type Out = R
        override val typ: TType = implicitly[ArgType[R]]
        override def apply(r: R): Rep[Out] = r.asRep
      }

    implicit def convNull: Aux[Null, Null] = new Converter[Null] { //todo will this ever be used?
      override type Out = Null
      override def apply(r: Null): Rep[Out] = new Rep[Null] { override val tree = Literal(null) }
      override val typ: TType = implicitly[ArgType[Null]]
    }
  }

  trait Productized[H <: HList, R] {
    def apply(h: H): R

    def pure[Arg <: HList, T](implicit converter: Converter.Aux[R, T],
                              fnFromProduct: FnFromProduct[H => Expression],
                              translation: MapArgs.Aux[H, Arg]) = to(this)
  }

  implicit def productize[F, H <: HList, R](f: F)(implicit fnToProduct: FnToProduct.Aux[F, H => R]) = new Productized[H, R] {
    override def apply(h: H): R = fnToProduct(f)(h)
  }

  //seems that the compiler has problem inferring R: NotLifted, thus the productized object (one less jump)
  def to[H <: HList, R, Arg <: HList, T](f: Productized[H, R])(implicit
                                                  converter: Converter.Aux[R, T], fnFromProduct: FnFromProduct[H => Expression],
                                                  translation: MapArgs.Aux[H, Arg]) =
    new CallWord[T, Arg](fnFromProduct((x: H) => converter.apply(f(x)).tree), converter.typ)

  implicit class RepMap[T, A](arg: Rep[T])(implicit translation: MapArgs.Aux[A :: HNil, T :: HNil]) {
    def liftMap[B, R](f: A => B)(implicit converter: Converter.Aux[B, R]): Rep[R] =
      new Rep[R] {
        override val tree: Expression = ApplyNative((x: A) => converter.apply(f(x)).tree, TupleConstructor(Vector(arg.tree)))(converter.typ)
      }
  }
}
