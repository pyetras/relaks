package fwb.ast

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.Ops
/**
 * Created by Pietras on 10/04/15.
 */
trait Types { this: ASTNodes =>
  sealed trait TType
  trait NumType
  trait SupPosType

  sealed trait RootArgType[T] extends TType { self =>
    def supPosType : SupPosArgType[T] = new SupPosArgType[T] {
      override val insideType: RootArgType[T] = self
    }
  }

  sealed trait ArgType[T] extends RootArgType[T]

  class SupPos[T] extends Serializable

  sealed trait SupPosArgType[T] extends RootArgType[SupPos[T]] with SupPosType {
    val insideType: RootArgType[T]
    override def toString = s"SupPosArgType[?]"
  }

  class ScalaType[T](implicit val classTag: ClassTag[T]) extends ArgType[T] {
    override def toString = s"ScalaType[${classTag.runtimeClass.getSimpleName}]"
  }

  class ScalaNumType[T](implicit val tag: ClassTag[T]) extends ScalaType[T] with NumType

  object UnknownType extends TType

  trait ScalaTypeImplis {
    implicit val boolType = new ScalaType[Boolean]
    implicit val stringType = new ScalaType[String]
    implicit val intType = new ScalaNumType[Int]
    implicit val doubleType = new ScalaNumType[Double]
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

  trait ConstantOps {
    import Constants.Constant
    implicit def fromScalaType[T](i: T)(implicit ev: ScalaType[T]): Constant = new Constant(i)
    implicit def fromScalaSeq[T](s: Seq[T])(implicit ev: ScalaType[T]): Lst = Lst(s.toList)
  }

}

