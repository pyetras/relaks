package relaks.lang.ast

import shapeless.ops.hlist.{ToTraversable, Length}
import shapeless.ops.nat.ToInt
import scala.reflect.runtime.universe._
import shapeless.syntax.NatOps
import shapeless.{Nat, HList}
import shapeless.nat._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.Ops
import scala.collection.mutable
/**
 * Created by Pietras on 10/04/15.
 */
trait Types { this: ASTNodes =>
  sealed trait TType {
    override def equals(other: Any) = ClassTag(other.getClass) == ClassTag(this.getClass)
  }
  trait NumType

  sealed abstract class ArgType[T: ClassTag] extends TType { self =>
    override def toString = s"$containerName[$typeArgName]"
    def containerName: String = {
      def findNonAnon(kls: Class[_]) : String = {
        val name = kls.getSimpleName
        if (name.startsWith("$anon")) findNonAnon(kls.getSuperclass)
        else name
      }
      findNonAnon(this.getClass)
    }
    def typeArgName: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  }


  sealed abstract class UnliftedArgType[T: ClassTag] extends ArgType[T] {
  }

  sealed trait CompoundType

  sealed class ListType[T: ClassTag] extends UnliftedArgType[List[T]] with CompoundType

  final class Tup[+T <: HList]

  sealed abstract class TupType[T <: HList : TypeTag] extends UnliftedArgType[Tup[T]] with CompoundType {
    type LUB
    val length: Int
    val lowerBound: ArgType[LUB]
    val productTypes: Vector[TType]
    override def containerName = s"Tup$length"
    override def typeArgName = implicitly[TypeTag[T]].tpe.dealias.toString
  }

  sealed class Table
  sealed class NTable[N <: Nat] extends Table
  final class TypedTable[T <: HList] extends NTable[_1]

  sealed class UntypedTableType extends UnliftedArgType[Table] {
    var constraints = Vector.empty[Any]
  }

  sealed abstract class TypedTableType[T <: HList : TypeTag] extends UnliftedArgType[TypedTable[T]] with CompoundType {
    val length: Int
    val colNames: Vector[String]
    override def containerName = s"Table$length"
    override def typeArgName = implicitly[TypeTag[T]].tpe.dealias.toString
  }

  type TupleLU[H <: HList, LU] = ToTraversable.Aux[H, List, LU] //TODO: make it more efficient and preserve classtag

  sealed abstract class SimpleArgType[T: ClassTag] extends UnliftedArgType[T]

  sealed class ScalaType[T: ClassTag] extends SimpleArgType[T]

  class ScalaNumType[T : ClassTag] extends ScalaType[T] with NumType

  object UnknownType extends TType {
  }

  trait ScalaTypeImplis {
    implicit val boolType = new ScalaType[Boolean]
    implicit val stringType = new ScalaType[String]
    implicit val intType = new ScalaNumType[Int]
    implicit val doubleType = new ScalaNumType[Double]
    implicit val nullType = new ScalaType[Null]
    implicit val longType = new ScalaType[Long]

    implicit def listType[T: ClassTag](implicit typ: ArgType[T]): ListType[T] = new ListType[T]

    abstract class TupTypeConstructor[T <: HList : TypeTag](n: Int) {
      type LU
      val luCT: ClassTag[LU]

      def apply(inner: Vector[TType]): TType = {

        implicit val ct = luCT
        val lut = new UnliftedArgType[LU] {}//FIXME np listy beda kiepskie

        val typ = new TupType[T] {
          override val length: Int = n
          override type LUB = LU
          override val lowerBound: ArgType[LU] = lut
          override val productTypes: Vector[TType] = inner
        }
        typ
      }
    }
    implicit def tupleTypeConstructor[H <: HList : TypeTag, N <: Nat, LUB: ClassTag]
      (implicit len: Length.Aux[H, N],
       ti: ToInt[N],
       tt: TupleLU[H, LUB]): TupTypeConstructor[H] =
      new TupTypeConstructor[H](Nat.toInt[N]) {
        override type LU = LUB
        override val luCT: ClassTag[LU] = implicitly[ClassTag[LUB]]
      }
  }

  trait Typed { this: Tree =>
    private var _tpe: TType = UnknownType
    def assignType(typ: TType) = _tpe = typ
    def tpe = _tpe

    override def toString = if (_tpe != UnknownType) { s"$mainToString : ${_tpe.toString}" } else { s"$mainToString: ?" }

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

  /**
   * Tree type extractor
    */
  object :@ {
    def unapply[T <: Tree with Typed](t: T) = Some((t, t.tpe))
  }
}
