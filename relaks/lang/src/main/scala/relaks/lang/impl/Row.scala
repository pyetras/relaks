package relaks.lang.impl

import com.twitter.bijection.Injection
import relaks.lang.ast.TType
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
}

class Row(val values: Vector[Any], private val schema: Schema) extends UntypedRow {
  def length = values.length

  lazy val colNames = schema.schema.map(_._1)

  def apply(colname: String) = {
    values(schema.names(colname))
  }

  override def apply(i: Int) = {
    values(i)
  }

  override def apply[T: ClassTag](rng: Range)(implicit injection: Injection[T, String]): BrVec[T] = {
    val arr = Array.newBuilder[T]
    arr.sizeHint(rng)
    for (i <- rng) arr += injection.invert(values(i).asInstanceOf[String]).get
    BrVec(arr.result())
  }

  override def toString: String = values.toString()
}
