package relaks.lang.ast

import breeze.math.Field
import relaks.lang.dsl.utils.TupleLU
import shapeless.nat._
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.{HNil, HList, Nat}

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalaz.{Scalaz, Order}
import scalaz.syntax.Ops
/**
 * Created by Pietras on 10/04/15.
 */

sealed trait TType {
  def canEqual(other: Any) = ClassTag(other.getClass) == ClassTag(this.getClass)
  def ct: WeakTypeTag[_]
  val isUnknown = false
}
trait NumType

sealed abstract class ArgType[T: WeakTypeTag](implicit val order: Order[T] = null) extends TType { self =>
  override def toString = s"$containerName[$typeArgName]"

  override lazy val ct = implicitly[WeakTypeTag[T]]

  def containerName: String = {
    def findNonAnon(kls: Class[_]) : String = {
      val name = kls.getSimpleName
      if (name.startsWith("$anon")) findNonAnon(kls.getSuperclass)
      else name
    }
    findNonAnon(this.getClass)
  }
  def typeArgName: String = ct.tpe.dealias.toString

  override def equals(obj: scala.Any): Boolean = canEqual(obj) && obj.asInstanceOf[ArgType[_]].ct.tpe =:= ct.tpe
}

sealed class NativeArgType[T: WeakTypeTag](override implicit val order: Order[T] = null) extends ArgType[T]

sealed abstract class LiftedArgType[T: WeakTypeTag] extends ArgType[T] //represents types explicitly lifted to reps

sealed trait CompoundType

sealed class ListType[T: ArgType] extends LiftedArgType[List[T]] with CompoundType {
  val childType = implicitly[ArgType[T]]

  override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[ListType[_]].childType == childType
}

final class Tup[+T <: HList]

sealed abstract class TupType[T <: HList : WeakTypeTag] extends LiftedArgType[Tup[T]] with CompoundType {
  type LUB
  val length: Int
  val lowerBound: ArgType[LUB]
  val childrenTypes: Vector[TType]
  override def containerName = s"Tup$length"
  override def typeArgName = implicitly[WeakTypeTag[T]].tpe.dealias.toString
}

object TupType {
  def fromElements[T <: HList : WeakTypeTag](exprs: Vector[Expression]): TupType[T] = new TupType[T] {
    override type LUB = Any //TODO
    override val length: Int = exprs.size
    override val childrenTypes: Vector[TType] = exprs.map(_.tpe)
    override val lowerBound: ArgType[LUB] = ScalaTypes.anyType
  }
}

sealed class Table
final class UntypedTable extends Table
final class TypedTable[T] extends Table

sealed class UntypedTableType extends LiftedArgType[UntypedTable] {
  var constraints = Vector.empty[Any]

  override def toString: String = s"UTTable"
}

sealed abstract class TypedTableType[T <: HList : TypeTag] extends LiftedArgType[TypedTable[T]] with CompoundType {
  val length: Int
  val colNames: Vector[String]
  override def containerName = s"UntypedTable$length"
  override def typeArgName = implicitly[TypeTag[T]].tpe.dealias.toString
}

sealed abstract class SimpleArgType[T: ClassTag] extends LiftedArgType[T]

sealed class ScalaType[T: ClassTag](override implicit val order: Order[T] = null) extends SimpleArgType[T]

class ScalaNumType[T : ClassTag : Order](implicit val field: Field[T]) extends ScalaType[T] with NumType

object UnknownType extends TType {
  override val ct: WeakTypeTag[_] = null
  override val isUnknown: Boolean = true
  import Scalaz._
  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[AnyRef] ? (this eq obj.asInstanceOf[AnyRef]) | false
}

object ScalaTypes {
  import scalaz.Scalaz._
  import breeze.math.Field._

  val boolType = new ScalaType[Boolean]
  val stringType = new ScalaType[String]
  val intType = new ScalaNumType[Int]
  val doubleType = new ScalaNumType[Double]
//  val nullType = new ScalaType[Null]
  val longType = new ScalaNumType[Long]
  val anyType = new ScalaType[Any]
}

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

  implicit def listType[T: WeakTypeTag](implicit typ: ArgType[T]): ListType[T] = new ListType[T]

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

      val lut = new LiftedArgType[LU] {}//FIXME np listy beda kiepskie

      val typ = new TupType[T] {
        override val length: Int = n
        override type LUB = LU
        override val lowerBound: ArgType[LU] = lut
        override val childrenTypes: Vector[TType] = inner
      }
      typ
    }
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
