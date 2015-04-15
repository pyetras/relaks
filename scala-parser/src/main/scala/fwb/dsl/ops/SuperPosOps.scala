package fwb.dsl.ops

import fwb.dsl.AST._

/**
 * Created by Pietras on 13/04/15.
 */
sealed abstract class SuperPosed[T](implicit ev: ScalaType[T]) {
  def toTree: NondetChoice
  def tpe = new SuperPosGenType[T] { val insideType = ev }
}

case class SuperPosRange[T](from: T, to: T)(implicit typ: ScalaType[T]) extends SuperPosed[T] {
  def toTree = NondetChoiceRange(Literal(from), Literal(to)) // TODO: źle - to musi zostac nazwane. var?
}

case class SuperPosChoice[T](choice: List[T])(implicit typ: ScalaType[T]) extends SuperPosed[T] {
  def toTree = NondetChoiceList(Literal(choice))
}

trait SuperPosOps {
  object choose extends ToTypedTreeOps {
    trait Between[T] {
      val from: T
      def and(t: T)(implicit typ: ScalaType[T]) = SuperPosRange(from, t)
    }
    def between[T](frm: T) = new Between[T] { val from = frm }
    def from[T](from: Seq[T])(implicit typ: ScalaType[T]) = SuperPosChoice(from.toList)
  }
}

trait SuperPosMapper[B1, B2, BR, P1, P2, PR] extends ToTypedTreeOps {
  val lift = true
  def toComputation(name: String, args: Expression*)(implicit tpe: ArgType[BR]): Rep[PR] = new Rep[PR] {
    override def tree: TTree = Apply(name, args:_*)(liftedType(tpe))
  }

  def liftedType(tpe: ArgType[BR]): ArgType[PR] =
    (if (lift) { tpe.supPosType } else { tpe }).asInstanceOf[ArgType[PR]]
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