package fwb.dsl.extensions

import fwb.dsl._
import AST._

import org.kiama.attribution.Attribution._
import scala.language.implicitConversions

/**
 * Created by Pietras on 13/04/15.
 */
trait SuperPosExtensions extends ListExtensions with Symbols {
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

  private[this] implicit def superPosedToRep[B1](sp: SuperPosed[B1])(implicit tpe: UnliftedArgType[B1]): Rep[B1] = {
    val t: Atom = sp.toTree(sp.tpe)
    new Rep[B1] {
      override val tree: Expression = t
    }
  }

  object choose extends ToTypedTreeOps {
    trait Between[T] {
      val from: T
      def and(t: T)(implicit typ: ScalaType[T]): Rep[T] = SuperPosRange(from, t)
    }
    def between[T](frm: T) = new Between[T] { val from = frm }
    def from[T : UnliftedArgType](from: Rep[List[T]]): Rep[T] = SuperPosChoice(from) //FIXME: should accept superposed types
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

  def showSpace(superPos: Rep[Any]) = {
    assert(superPos.getTpe.isSuperPosed)
    initTree(superPos.tree)
    superPos.tree->superPosDeps collect {
      case Expr(NondetChoiceList(Expr(ListConstructor(lst)))) => lst.toString()
      case Expr(NondetChoiceList(Expr(Literal(x)))) => x.toString()
      case Expr(NondetChoiceRange(l, r)) => s"{${l.value} .. ${r.value}}"
    }
  }
}

private[extensions] class SuperPosMapper[B1, B2, BR] {
  import AST.syntax._
  def toRep(name: Expression, args: Expression*)(implicit tpe: ArgType[BR]): Rep[BR] = {
    // if any of the types is superposed, lift the return type
    val lift = args.exists(_.tpe.isSuperPosed) || name.tpe.isSuperPosed
    new Rep[BR] {
      override val tree: Expression = Apply(name, args.toList)(if (lift) { tpe.supPosType } else { tpe })
    }
  }

}
