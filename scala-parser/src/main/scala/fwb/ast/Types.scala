package fwb.ast

import fwb.dsl.SuperPosed

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.Ops
/**
 * Created by Pietras on 10/04/15.
 */
trait Types { this: ASTNodes =>
  sealed trait TType {
    override def equals(other: Any) = ClassTag(other.getClass) == ClassTag(this.getClass)
  }
  trait NumType
  trait SuperPosType

  sealed trait ArgType[T] extends TType { self =>
    def supPosType : SuperPosResult[T] = new SuperPosResult[T] {
      override val insideType: ArgType[T] = self
    }
  }

  sealed trait SimpleArgType[T] extends ArgType[T]

  final class SuperPos[T] 

  sealed trait SuperPosArgType[T] extends ArgType[SuperPos[T]] with SuperPosType {
    val insideType: ArgType[T]
    override def toString = s"SuperPosArgType[?]"
  }

  sealed trait SuperPosResult[T] extends SuperPosArgType[T]

  trait SuperPosScalaType[T] extends SuperPosArgType[T] //?

  class ScalaType[T](implicit val classTag: ClassTag[T]) extends SimpleArgType[T] {
    override def toString = s"ScalaType[${classTag.runtimeClass.getSimpleName}]"
  }

  class ScalaNumType[T](implicit val tag: ClassTag[T]) extends ScalaType[T] with NumType

  object UnknownType extends TType

  trait ScalaTypeImplis {
    implicit val boolType = new ScalaType[Boolean]
    implicit val stringType = new ScalaType[String]
    implicit val intType = new ScalaNumType[Int]
    implicit val doubleType = new ScalaNumType[Double]
    implicit val nullType = new ScalaType[Null]
    implicit val longType = new ScalaType[Long]

    implicit def listType[T] = new ScalaType[List[T]] //TODO
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
