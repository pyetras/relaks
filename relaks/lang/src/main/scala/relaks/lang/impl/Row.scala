package relaks.lang.impl

import com.twitter.bijection.Injection
import relaks.lang.ast.{ArgType, TType}
import breeze.linalg.{Vector => BrVec}
import scalikejdbc.TypeBinder

import scala.reflect.ClassTag
/**
 * Created by Pietras on 06/07/15.
 */
case class Schema(schema: Vector[(String, TType)]) {
  val names: Map[String, Int] = schema.indices.map(i => schema(i)._1 -> i).toMap
}

trait UntypedRow {
  def apply[T: ClassTag](rng: Range)(implicit injection: Injection[T, String]): BrVec[T]
  def apply(i: Int): Any
  def apply(colname: String): Any
  def get[T: ArgType](i: Int): T
  def get(t: ArgType[_])(name: String): Any
}

trait Row extends UntypedRow {
  def colNames: Vector[String]
  def values: Vector[Any]
}

class VectorRow(override val values: Vector[Any], private val schema: Schema) extends Row {
  override lazy val colNames = schema.schema.map(_._1)

  def length = values.length
  override def apply(colname: String) = {
    values(schema.names(colname))
  }
  override def apply(i: Int) = {
    values(i)
  }
  override def apply[T: ClassTag](rng: Range)(implicit injection: Injection[T, String]): BrVec[T] = ??? //TODO
  override def toString: String = values.toString()
  override def get[T: ArgType](i: Int): T = apply(i).asInstanceOf[T]
  override def get(t: ArgType[_])(name: String): Any = apply(name)
}

class CsvAllRow(override val values: Vector[String], length: Int) extends Row {
  override lazy val colNames = (0 until length).map(i => s"columns[$i]").toVector

  private val namePattern = "columns\\[(\\d+)\\]".r
  private def getIx(colName: String) = namePattern.findFirstMatchIn(colName).get.group(1).toInt

  override def apply[T: ClassTag](rng: Range)(implicit injection: Injection[T, String]): BrVec[T] = {
    val arr = Array.newBuilder[T]
    arr.sizeHint(rng)
    for (i <- rng) arr += injection.invert(values(i)).get
    BrVec(arr.result())
  }

  override def apply(colname: String): Any = ???

  override def apply(i: Int): Any = values(i)

  override def toString: String = values.toString()

  override def get(t: ArgType[_])(name: String): Any = {
    implicit val typ = t
    val ix = getIx(name)
    get(ix)
  }

  override def get[T: ArgType](i: Int): T = {
    implicitly[ArgType[T]].toText.invert(values(i)).get


  }
}