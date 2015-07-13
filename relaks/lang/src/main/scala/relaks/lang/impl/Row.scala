package relaks.lang.impl

import relaks.lang.ast.TType

/**
 * Created by Pietras on 06/07/15.
 */
class Row(val values: Vector[Any], private val schema: Vector[(String, TType)]) {
  def length = values.length

  private val names: Map[String, Int] = schema.indices.map(i => schema(i)._1 -> i).toMap

  def apply(colname: String) = {
    values(names(colname))
  }

  def apply(i: Int) = {
    values(i)
  }

  override def toString: String = values.toString()
}
