package relaks.lang.dsl

import relaks.lang.ast._
import relaks.lang.dsl.utils._
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.{<:!<, HList, Nat}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._


import scalaz.Order

/**
 * Created by Pietras on 21.08.15.
 */
trait ScalaTypeImplis {
  implicit val boolType = ScalaTypes.boolType
  implicit val stringType = ScalaTypes.stringType
  implicit val intType = ScalaTypes.intType
  implicit val doubleType = ScalaTypes.doubleType
//  implicit val nullType = ScalaTypes.nullType
  implicit val longType = ScalaTypes.longType

  trait Not[A]
  type NotLifted[A] = Not[LiftedArgType[A]]

  implicit def notA[A] = new NotLifted[A] {}
  implicit def notNotA[A](implicit liftedArgType: LiftedArgType[A]) = new NotLifted[A] {} //adds ambiguity

//  implicit def otherType[T: WeakTypeTag](implicit notLifted: NotLifted[T]): NativeArgType[T] = new NativeArgType[T]
  implicit def otherType[T: WeakTypeTag](implicit lifted: LiftedArgType[T] = null, order: Order[T] = null): ArgType[T] = if (lifted != null) lifted else new NativeArgType[T]

  def listType[T: WeakTypeTag](implicit typ: ArgType[T]): ListType[T] = new ListType[T]

  object TupTypeConstructor {
    def apply[T <: HList: WeakTypeTag](implicit tupTypeConstructor: TupTypeConstructor[T]) = tupTypeConstructor
    implicit def tupleTypeConstructor[H <: HList: WeakTypeTag, N <: Nat, LUB : WeakTypeTag]
    (implicit len: Length.Aux[H, N],
     ti: ToInt[N],
     tt: TupleLU[H, LUB]): TupTypeConstructor[H] =
      new TupTypeConstructor[H](Nat.toInt[N]) {
        override type LU = LUB
        override val luCT: WeakTypeTag[LU] = implicitly[WeakTypeTag[LUB]]
      }
  }

  abstract class TupTypeConstructor[T <: HList : WeakTypeTag](n: Int) {
    type LU
    implicit val luCT: WeakTypeTag[LU]

    def apply(inner: Vector[TType]): TType = {

      val lut = new LiftedArgType[LU] //FIXME np listy beda kiepskie

      new TupType[T](n, lut, inner)
    }
  }
}
