package fwb.ast

import shapeless.ops.hlist.{ToTraversable, Length}
import shapeless.ops.nat.ToInt
import scala.reflect.runtime.universe._
import shapeless.syntax.NatOps
import shapeless.{Nat, HList}
import shapeless.nat._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.Ops
/**
 * Created by Pietras on 10/04/15.
 */
trait Types { this: ASTNodes =>
  sealed trait TType {
    val isSuperPosed: Boolean
    def unlift: TType //return unsuperposed version of this type
    override def equals(other: Any) = ClassTag(other.getClass) == ClassTag(this.getClass)
  }
  trait NumType
  trait SuperPosType

  sealed trait ArgType[T] extends TType { self =>
    def supPosType : SuperPosResult[T] = new SuperPosResult[T] {
      override val insideType: ArgType[T] = self
    }
  }

  sealed trait SuperPosArgType[T] extends ArgType[T] with SuperPosType {
    override def unlift: TType = insideType
    override final val isSuperPosed: Boolean = true
    val insideType: ArgType[T]
    override def toString = s"SuperPosArgType[?]"
  }

  sealed trait SuperPosResult[T] extends SuperPosArgType[T]

  trait SuperPosGenType[T] extends SuperPosArgType[T]

  sealed trait UnliftedArgType[T] extends ArgType[T] {
    override def unlift: TType = this
    override final val isSuperPosed: Boolean = false
  }

  sealed trait CompoundType

  sealed class ListType[T] extends UnliftedArgType[List[T]] with CompoundType

  final class Prod[+T <: HList]

  sealed abstract class ProdType[T <: HList : TypeTag] extends UnliftedArgType[Prod[T]] with CompoundType {
    type LUB
    val length: Int
    val lowerBound: ArgType[LUB]
    val productTypes: Seq[TType]
    override def toString = s"Prod$length[${implicitly[TypeTag[T]].tpe.dealias}]"
  }

  type ProductLU[H <: HList, LU] = ToTraversable.Aux[H, List, LU] //TODO: make it more efficient and preserve classtag

  sealed trait SimpleArgType[T] extends UnliftedArgType[T]

  sealed class ScalaType[T : ClassTag] extends SimpleArgType[T] {
    override def toString = s"ScalaType[${implicitly[ClassTag[T]].runtimeClass.getSimpleName}]"
  }

  class ScalaNumType[T : ClassTag] extends ScalaType[T] with NumType

  object UnknownType extends TType {
    override def unlift: TType = this
    override val isSuperPosed: Boolean = false
  }

  trait ScalaTypeImplis {
    implicit val boolType = new ScalaType[Boolean]
    implicit val stringType = new ScalaType[String]
    implicit val intType = new ScalaNumType[Int]
    implicit val doubleType = new ScalaNumType[Double]
    implicit val nullType = new ScalaType[Null]
    implicit val longType = new ScalaType[Long]

    implicit def listType[T](implicit typ: ArgType[T]): ListType[T] = new ListType[T]

    class ProdTypeConstructor[T <: HList : TypeTag, LU](n: Int) {
      def apply(inner: Seq[TType]): TType = {
        val lift = inner.exists(_.isSuperPosed)
        val lut = new UnliftedArgType[LU] {}//FIXME np listy beda kiepskie
        val typ = new ProdType[T] {
          override val length: Int = n
          override type LUB = LU
          override val lowerBound: ArgType[LU] = if (lift) { lut.supPosType } else { lut }
          override val productTypes: Seq[TType] = inner
        }
        if (lift) { typ.supPosType } else { typ }
      }
    }
    implicit def productTypeConstructor[H <: HList : TypeTag, N <: Nat, LU]
      (implicit len: Length.Aux[H, N],
       ti: ToInt[N],
       tt: ProductLU[H, LU]): ProdTypeConstructor[H, LU] =
      new ProdTypeConstructor[H, LU](Nat.toInt[N])
  }

  trait Typed { this: Tree =>
    private var _tpe: TType = UnknownType
    def assignType(typ: TType) = _tpe = typ
    def tpe = _tpe

    override def toString = if (_tpe != UnknownType) { s"$mainToString : ${_tpe.toString}" } else { mainToString }
  }

  trait TypedTreeOps[T <: Tree with Typed] extends Ops[T] {
    def apply(typ: TType) = {
      self.assignType(typ)
      self
    }
  }

  trait ToTypedTreeOps {
    implicit def TypedTreeToTypedTreeOps[T <: Tree with Typed](tree: T): TypedTreeOps[T] = new TypedTreeOps[T] {
      def self = tree
    }
  }
}
