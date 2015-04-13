package fwb.ast

/**
 * Created by Pietras on 24/03/15.
 */


import scala.language.implicitConversions

sealed trait Value {
  type ValueT
  val v: ValueT
}

final case class Lst(v: List[Any]) extends Value {
  type ValueT = List[Any]
}

object Constants {
  final val NoTag = 0
  final val UnitTag = 1
  final val BooleanTag = 2
  final val IntTag = 6
  final val LongTag = 7
  final val DoubleTag = 9
  final val StringTag = 10
  final val NullTag = 11

  final case class Constant(val v: Any) extends Value {
    type ValueT = Any

    val tag: Int = v match {
      case null => NullTag
      case x: Unit => UnitTag
      case x: Boolean => BooleanTag
      case x: Int => IntTag
      case x: Long => LongTag
      case x: Double => DoubleTag
      case x: String => StringTag
      case _ => throw new Error("bad constant value: " + v)
    }

  }
}
