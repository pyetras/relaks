package fwb.dsl.extensions

import fwb.dsl._
import AST._
import AST.syntax._

import scala.language.implicitConversions

/**
 * Created by Pietras on 10/04/15.
 */
trait Operations[B1] {
  val arg1: Rep[B1]

  val op = new  {
    def to[BR] = new SuperPosMapper[B1, B1, BR]
    def arg2[B2] = new {
      def to[BR] = new SuperPosMapper[B1, B2, BR]
    }
  }

  val tree = arg1.tree

  implicit protected[this] def b1Type = (arg1.getTpe match {
    case o: SuperPosArgType[_] => o.insideType
    case b => b
  }).asInstanceOf[ArgType[B1]]
}
