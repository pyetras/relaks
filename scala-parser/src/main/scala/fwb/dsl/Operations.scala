package fwb.dsl

import fwb.dsl.AST._
import AST.syntax._
import fwb.ast.Constants.Constant
import scala.language.implicitConversions

/**
 * Created by Pietras on 10/04/15.
 */
trait Operations[B1, P1] {
  val operand: Computation[P1]
  type or = OpResolverDSL.arg1[B1, P1]
  def tree = operand.tree
}

class AnyOperations[B1, P1](val operand: Computation[P1]) extends Operations[B1, P1] {
//  def ===[P2, PR](arg: Computation[P2])(implicit o: )
}

class NumericOperations[B1, P1](val operand: Computation[P1]) extends Operations[B1, P1] {
  def < [P2, PR](arg: Computation[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) =
   o.toComputation("<", operand.tree, arg.tree)
}

trait SupPosMapper[B1, B2, BR, P1, P2, PR] extends ToTypedTreeOps {
  val lift = true
  def toComputation(name: String, args: Expression*)(implicit tpe: RootArgType[BR]): Computation[PR] = new Computation[PR] {
    override def tree: TTree = Apply(name, args:_*)(liftedType(tpe))
  }

  def liftedType(tpe: RootArgType[BR]): RootArgType[PR] =
    (if (lift) { tpe.supPosType } else { tpe }).asInstanceOf[RootArgType[PR]]
}

trait SupPosMapperImplis {
  implicit def getSupPosMapperTT[B1, B2 : ArgType, BR] = new SupPosMapper[B1, B2, BR, B1, B2, BR] { override val lift = false }
  implicit def getSupPosMapperTS[B1, B2 : ArgType, BR] = new SupPosMapper[B1, B2, BR, B1, SupPos[B2], SupPos[B2]] {}
  implicit def getSupPosMapperST[B1, B2 : ArgType, BR] = new SupPosMapper[B1, B2, BR, SupPos[B1], B2, SupPos[BR]] {}
  implicit def getSupPosMapperSS[B1, B2 : ArgType, BR] = new SupPosMapper[B1, B2, BR, SupPos[B1], SupPos[B2], SupPos[BR]] {}
}

trait OperationsSyntax extends ToTypedTreeOps {
  implicit def anyToComputation[B1](x: B1)(implicit tpe: ArgType[B1]): Computation[B1] = new Computation[B1] {
    override def tree: TTree = Literal(Constant(x))(tpe)
  }
//  implicit def supPosToComputation[B1](x: SupPos[B1]) : Computation[SupPos[B1]] = x.toComputation
  implicit def addSupPosNumericOps[B1, T](operand: T)(implicit ev1: T => Computation[SupPos[B1]], ev2: ArgType[B1] with NumType) =
    new NumericOperations[B1, SupPos[B1]](ev1(operand))
  implicit def addNumericOps[B1, T](operand: T)(implicit ev1: T => Computation[B1], ev2: ArgType[B1] with NumType) =
    new NumericOperations[B1, B1](operand)
}

object OpResolverDSL {
  type arg1[B1, P1] = {
    type to[BR, PR] = SupPosMapper[B1, B1, BR, P1, P1, PR]
    type arg2[B2, P2] = {
      type to[BR, PR] = SupPosMapper[B1, B2, BR, P1, P2, PR]
    }
  }
}