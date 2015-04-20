package fwb.dsl.extensions

import fwb.dsl._
import AST._
import AST.syntax._

import scala.language.implicitConversions

/**
 * Created by Pietras on 10/04/15.
 */
trait Operations[B1, P1] {
  val arg1: Rep[P1]
  type or = OpResolverDSL.arg1[B1, P1]
  def tree = arg1.tree

  protected[this] implicit def p1Type = arg1.getTpe.asInstanceOf[ArgType[P1]] // :(
  implicit protected[this] def b1Type = (arg1.getTpe match {
    case o: SuperPosArgType[_] => o.insideType
    case b => b
  }).asInstanceOf[ArgType[B1]]
  implicit protected[this] def optionType = (arg1.getTpe match {
    case o: SuperPosArgType[_] => o
    case b => b.asInstanceOf[ArgType[B1]].supPosType
  }).asInstanceOf[ArgType[SuperPos[B1]]]

}
