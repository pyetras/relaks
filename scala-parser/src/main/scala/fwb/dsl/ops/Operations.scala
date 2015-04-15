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

class AnyOperations[B1, P1](val operand: Rep[P1]) extends Operations[B1, P1] {
//  def ===[P2, PR](arg: Rep[P2])(implicit o: )
}

class NumericOperations[B1, P1](val operand: Rep[P1]) extends Operations[B1, P1] {
  def < [P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) =
   o.toComputation(Stdlib.<, operand.tree, arg.tree)
}


trait OperationsSyntax extends ToTypedTreeOps with Symbols {
  implicit def addSupPosNumericOps[B1, T](operand: T)(implicit ev1: T => Rep[SuperPos[B1]], ev2: LiftedArgType[B1] with NumType) =
    new NumericOperations[B1, SuperPos[B1]](ev1(operand))
  implicit def addNumericOps[B1, T](operand: T)(implicit ev1: T => Rep[B1], ev2: LiftedArgType[B1] with NumType) =
    new NumericOperations[B1, B1](operand)
}

