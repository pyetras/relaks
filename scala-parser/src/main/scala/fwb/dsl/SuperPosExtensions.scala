package fwb.dsl

import fwb.ast._
import fwb.dsl.AST._

/**
 * Created by Pietras on 13/04/15.
 */

case class SuperPosRange[T](from: T, to: T)(implicit tpe: ScalaType[T]) extends SuperPosScalaType[T] {
  val insideType = tpe
}

case class SuperPosChoice[T](choice: Traversable[T])(implicit tpe: ScalaType[T]) extends SuperPosScalaType[T] {
  val insideType = tpe
}

trait SuperPosOps {
  object choose extends ToTypedTreeOps {
    trait Between[T] {
      val from: T
      def and(t: T)(implicit ev: T => Value, typ: ScalaType[T]) = SuperPosRange(from, t)
    }
    def between[T](frm: T) = new Between[T] { val from = frm }
    def from[T](from: Seq[T])(implicit typ: ScalaType[T]) = SuperPosChoice(from)
  }
}

trait SuperPosMapper[B1, B2, BR, P1, P2, PR] extends ToTypedTreeOps {
  val lift = true
  def toComputation(name: String, args: Expression*)(implicit tpe: ArgType[BR]): Rep[PR] = new Rep[PR] {
    override def tree: TTree = Apply(name, args:_*)(liftedType(tpe, this.tree))
  }

  def liftedType(tpe: ArgType[BR], tree: => TTree): ArgType[PR] =
    (if (lift) { tpe.supPosType(tree) } else { tpe }).asInstanceOf[ArgType[PR]]
}

trait SuperPosMapperImplis {
  implicit def getSupPosMapperTT[B1, B2 : SimpleArgType, BR] = new SuperPosMapper[B1, B2, BR, B1, B2, BR] { override val lift = false }
  implicit def getSupPosMapperTS[B1, B2 : SimpleArgType, BR] = new SuperPosMapper[B1, B2, BR, B1, SuperPos[B2], SuperPos[B2]] {}
  implicit def getSupPosMapperST[B1, B2 : SimpleArgType, BR] = new SuperPosMapper[B1, B2, BR, SuperPos[B1], B2, SuperPos[BR]] {}
  implicit def getSupPosMapperSS[B1, B2 : SimpleArgType, BR] = new SuperPosMapper[B1, B2, BR, SuperPos[B1], SuperPos[B2], SuperPos[BR]] {}
}

object OpResolverDSL {
  type arg1[B1, P1] = {
    type to[BR, PR] = SuperPosMapper[B1, B1, BR, P1, P1, PR]
    type arg2[B2, P2] = {
      type to[BR, PR] = SuperPosMapper[B1, B2, BR, P1, P2, PR]
    }
  }
}