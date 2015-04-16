package fwb.dsl.ops

import fwb.dsl._
import AST._
import AST.syntax._

import scala.language.implicitConversions

/**
 * Created by Pietras on 10/04/15.
 */
trait Operations[B1, P1] {
  val operand: Rep[P1]
  type or = OpResolverDSL.arg1[B1, P1]
  def tree = operand.tree
}
