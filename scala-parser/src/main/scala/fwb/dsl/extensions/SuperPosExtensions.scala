package fwb.dsl.extensions

import fwb.dsl._
import AST._

import org.kiama.attribution.Attribution._
import scala.language.implicitConversions

/**
 * Created by Pietras on 13/04/15.
 */
trait SuperPosExtensions extends ListExtensions with Symbols with SuperPosMapperImplis {
  sealed abstract class SuperPosed[T](implicit ev: ArgType[T]) {
    def toTree: NondetChoice
    def tpe = new SuperPosGenType[T] { val insideType = ev }
  }

  case class SuperPosRange[T](from: T, to: T)(implicit typ: ScalaType[T]) extends SuperPosed[T] {
    def toTree = NondetChoiceRange(Literal(from), Literal(to)) // TODO: źle - to musi zostac nazwane.
  }

  case class SuperPosChoice[T](choice: Rep[ArgType[List[T]]])(implicit typ: ArgType[T]) extends SuperPosed[T] {
    def toTree = NondetChoiceList(choice.tree)
  }
  object SuperPosChoice {
    def apply[T](choice: Rep[List[T]])(implicit ev: ArgType[T]) : SuperPosed[T] = {
      new SuperPosed[T] {
        override def toTree = NondetChoiceList(choice.tree)
      }
    }
  }

  implicit def superPosedToRep[B1](sp: SuperPosed[B1])(implicit tpe: UnliftedArgType[B1]): Rep[SuperPos[B1]] = {
    val t: Atom = sp.toTree(sp.tpe)
    new Rep[SuperPos[B1]] {
      override val tree: Expression = t
    }
  }

  object choose extends ToTypedTreeOps {
    trait Between[T] {
      val from: T
      def and(t: T)(implicit typ: ScalaType[T]): Rep[SuperPos[T]] = SuperPosRange(from, t)
    }
    def between[T](frm: T) = new Between[T] { val from = frm }
    def from[T : UnliftedArgType](from: Rep[List[T]]): Rep[SuperPos[T]] = SuperPosChoice(from) //FIXME: should accept superposed types
  }

  val superPosDeps: Expression => Set[Sym] = attr (node => node.tpe match {
    case t:SuperPosGenType[_] => Set(node.asInstanceOf[Sym])
    case _ => node match {
      case Expr(link) if link.hasChildren => link.children.foldLeft(Set[Sym]())((acc, child) =>
        acc ++ (child.asInstanceOf[Expression] -> superPosDeps)
      )
      case _ => Set()
    }
  })

  def showSpace[T](superPos: Rep[SuperPos[T]]) = {
    initTree(superPos.tree)
    superPos.tree->superPosDeps collect {
      case Expr(NondetChoiceList(Expr(ListConstructor(lst)))) => lst.toString()
      case Expr(NondetChoiceList(Expr(Literal(x)))) => x.toString()
      case Expr(NondetChoiceRange(l, r)) => s"{${l.value} .. ${r.value}}"
    }
  }
}

private[extensions] trait SuperPosMapper[B1, B2, BR, P1, P2, PR] extends ToTypedTreeOps {
  val lift = true
  def toRep(name: Expression, args: Expression*)(implicit tpe: ArgType[BR]): Rep[PR] = new Rep[PR] {
    override val tree: Expression = Apply(name, args.toList)(liftedType(tpe))
  }

  def liftedType(tpe: ArgType[BR]): ArgType[PR] =
    (if (lift) { tpe.supPosType } else { tpe }).asInstanceOf[ArgType[PR]]
}

trait SuperPosMapperImplis {
  implicit def getSupPosMapperTT[B1, B2 : UnliftedArgType, BR] = new SuperPosMapper[B1, B2, BR, B1, B2, BR] { override val lift = false }
  implicit def getSupPosMapperTS[B1, B2 : UnliftedArgType, BR] = new SuperPosMapper[B1, B2, BR, B1, SuperPos[B2], SuperPos[B2]] {}
  implicit def getSupPosMapperST[B1, B2 : UnliftedArgType, BR] = new SuperPosMapper[B1, B2, BR, SuperPos[B1], B2, SuperPos[BR]] {}
  implicit def getSupPosMapperSS[B1, B2 : UnliftedArgType, BR] = new SuperPosMapper[B1, B2, BR, SuperPos[B1], SuperPos[B2], SuperPos[BR]] {}
}

private[extensions] object OpResolverDSL {
  type arg1[B1, P1] = {
    type to[BR, PR] = SuperPosMapper[B1, B1, BR, P1, P1, PR]
    type arg2[B2, P2] = {
      type to[BR, PR] = SuperPosMapper[B1, B2, BR, P1, P2, PR]
    }
  }
}