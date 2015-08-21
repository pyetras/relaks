package relaks.lang.ast

import breeze.math
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

sealed class LiftedArgType[T: WeakTypeTag] extends ArgType[T] //represents types explicitly lifted to reps

sealed trait CompoundType

final class ListType[T: ArgType] extends LiftedArgType[List[T]] with CompoundType {
  val childType = implicitly[ArgType[T]]

  override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[ListType[_]].childType == childType
}

sealed class Tupl
sealed trait Tup[+T <: HList] extends Tupl
sealed trait UntypedTup extends Tupl

final class TupType[T <: HList : WeakTypeTag](val length: Int, val lowerBound: TType, val childrenTypes: Vector[TType]) extends LiftedArgType[Tup[T]] with CompoundType {
  override def containerName = s"Tup$length"
  override def typeArgName = implicitly[WeakTypeTag[T]].tpe.dealias.toString
}

object TupType {
  def fromElements[T <: HList : WeakTypeTag](exprs: Vector[Expression]): TupType[T] =
    new TupType[T](exprs.size, ScalaTypes.anyType /*TODO*/, exprs.map(_.tpe))
}

sealed trait Table
sealed trait UntypedTable extends Table
sealed trait TypedTable[T] extends Table
sealed trait UnfinishedGenTable[T] extends Table

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

class ScalaNumType[T : ClassTag : Order](implicit val field: math.Field[T]) extends ScalaType[T] with NumType

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
