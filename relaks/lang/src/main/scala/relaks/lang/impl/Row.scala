package relaks.lang.impl

import com.twitter.bijection.Injection
import relaks.lang.ast.{ScalaTypes, ArgType, TType}
import breeze.linalg.{Vector => BrVec}
import scalikejdbc.{WrappedResultSet, TypeBinder}
import shapeless.HList

import scala.reflect.ClassTag
/**
 * Created by Pietras on 06/07/15.
 */
case class Schema(schema: Vector[(String, TType)]) {
  lazy val namesCols: Map[String, Int] = schema.indices.map(i => schema(i)._1 -> i).toMap
  lazy val names: Seq[String] = schema.view.map(_._1)
  val length = schema.length
}

trait Row {
  private implicit val anyType = ScalaTypes.anyType

  def get[T: ArgType](i: Int): T
  def get[T: ArgType](name: String): T

  def apply(i: Int): Any = get(i)
  def apply(colname: String): Any = get(colname)

  def apply[T: ClassTag: ArgType](rng: Range): BrVec[T] = {
    val arr = Array.newBuilder[T]
    arr.sizeHint(rng)
    for (i <- rng) arr += get[T](i)
    BrVec(arr.result())
  }

  def colNames: Seq[String]
  def values: Seq[Any]

  override def toString: String = values.toString()
}

trait UntypedRow extends Row
trait TypedRow[H <: HList] extends Row

trait SchemaDef { this: Row =>
  protected val schema: Schema
  override def colNames = schema.names
  val length = schema.length
  protected def getIx(name: String) = schema.namesCols(name)
}

class VectorRow(override val values: Vector[Any], override protected val schema: Schema) extends UntypedRow with SchemaDef {
  override def get[T: ArgType](i: Int): T = values(i).asInstanceOf[T]
  override def get[T: ArgType](name: String): T = values(getIx(name)).asInstanceOf[T]
}

class JDBCRow(rs: WrappedResultSet, override protected val schema: Schema) extends UntypedRow with SchemaDef {
  private def typeBinder[T](implicit argType: ArgType[T]) = argType.typeBinder
  override def values: Seq[Any] = (1 to length).map(i => rs.any(i))
  override def get[T: ArgType](i: Int): T = rs.get[T](i + 1)(typeBinder[T])
  override def get[T: ArgType](name: String): T = rs.get[T](name)(typeBinder[T])
}

class CsvAllRow(override val values: Vector[String]) extends UntypedRow {
  private val length = values.length
  override val colNames = (0 until length).view.map(i => s"columns[$i]").toVector

  private val namePattern = "columns\\[(\\d+)\\]".r
  private def getIx(colName: String) = namePattern.findFirstMatchIn(colName).get.group(1).toInt

  override def get[T: ArgType](name: String): T = {
    val ix = getIx(name)
    get(ix)
  }

  override def get[T: ArgType](i: Int): T = {
    implicitly[ArgType[T]].toText.invert(values(i)).get
  }
}