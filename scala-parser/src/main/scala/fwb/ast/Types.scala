package fwb.ast

import shapeless.ops.hlist.Length
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
    override def equals(other: Any) = ClassTag(other.getClass) == ClassTag(this.getClass)
  }
  trait NumType
  trait SuperPosType

  sealed trait ArgType[T] extends TType { self =>
    def supPosType : SuperPosResult[T] = new SuperPosResult[T] {
      override val insideType: ArgType[T] = self
    }
  }

  final class SuperPos[+T]

  sealed trait SuperPosArgType[T] extends ArgType[SuperPos[T]] with SuperPosType {
    override final val isSuperPosed: Boolean = true
    val insideType: ArgType[T]
    override def toString = s"SuperPosArgType[?]"
  }

  sealed trait SuperPosResult[T] extends SuperPosArgType[T]

  trait SuperPosGenType[T] extends SuperPosArgType[T]

  sealed trait UnliftedArgType[T] extends ArgType[T] {
    override final val isSuperPosed: Boolean = false
  }

  sealed trait CompoundType

  sealed class ListType[T] extends UnliftedArgType[List[T]] with CompoundType

  final class Prod[+T <: HList]

  sealed abstract class ProdType[T <: HList : TypeTag] extends UnliftedArgType[Prod[T]] with CompoundType {
    val length: Int

    override def toString = s"Prod$length[${implicitly[TypeTag[T]].tpe.dealias}]"
  }
  sealed abstract class ProdNType[T <: HList : TypeTag, N <: Nat : ToInt] extends ProdType[T] {
    val length = Nat.toInt[N]
  }

  sealed trait SimpleArgType[T] extends UnliftedArgType[T]

  sealed class ScalaType[T : ClassTag] extends SimpleArgType[T] {
    override def toString = s"ScalaType[${implicitly[ClassTag[T]].runtimeClass.getSimpleName}]"
  }

  class ScalaNumType[T : ClassTag] extends ScalaType[T] with NumType

  object UnknownType extends TType {
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
    implicit def superPosedType[T](implicit typ: UnliftedArgType[T]): SuperPosArgType[T] = typ.supPosType
    implicit def productType[H <: HList : TypeTag, N <: Nat](implicit len: Length.Aux[H, N], ti: ToInt[N]): ProdNType[H, N] = new ProdNType[H, N] { }// TODO: check if types belong to dsl
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
