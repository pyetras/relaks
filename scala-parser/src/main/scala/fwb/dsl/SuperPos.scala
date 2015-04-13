package fwb.dsl

/**
 * Created by Pietras on 11/04/15.
 */
trait SuperPos[T] {

}

case class RangeSuperPos[T](from: T, to: T) extends SuperPos[T]

case class DetSuperPos[T](value: T) extends SuperPos[T]