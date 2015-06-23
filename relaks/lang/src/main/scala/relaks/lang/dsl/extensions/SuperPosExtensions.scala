package relaks.lang.dsl.extensions

import org.kiama.attribution.Attribution
import org.kiama.relation.{Tree => RelTree}
import relaks.lang.ast._
import relaks.lang.dsl._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.Scalaz._
import scalaz.{Scalaz, ValidationNel}

/**
 * Created by Pietras on 13/04/15.
 */
trait SuperPosExtensions extends ListExtensions with Symbols with SuperPosGenerators {

  def once[T](superPos: Rep[T]): Rep[T] = {
//    assert(superPos.tree->isSuperPosed) //should happen after initialisation

    new Rep[T] {
      override val tree: Expression = Once(superPos.tree)(superPos.getTpe)
    }
  }

}

trait SuperPosAnalysis extends Symbols with BaseCompiler {

  class SuperPosed(tree: RelTree[Expression, Expression]) extends Attribution { self =>
    val superPosDeps: Expression => Set[Sym] = {
      attr {
        case Some(sym) /> (_: NondetGenerator) => Set(sym)
        case _ /> (link) => tree.child(link).map(self.superPosDeps).foldLeft(Set.empty[Sym]){_ ++ _}
      }
    }

    def isSuperPosed(expr: Expression) = self.superPosDeps(expr).nonEmpty
  }

  //TODO attribution
  def showSpace(superPos: Expression): Map[Int, Any] = ??? /*{
    assert(superPos->isSuperPosed)
    (superPos->superPosDeps map {
      case sym @ Expr(NondetGeneratorList(Expr(lc @ ListConstructor(lst)))) => sym.asInstanceOf[Sym].name -> lc
      case sym @ Expr(NondetGeneratorList(Expr(ll @ Literal(x)))) => sym.asInstanceOf[Sym].name -> ll
      case sym @ Expr(NondetGeneratorRange(l, r)) => sym.asInstanceOf[Sym].name -> (l.value, r.value)
    }).toMap
  }*/

  override protected def doAnalyze(root: Expression): ValidationNel[String, Unit] = root match {
    case n @ Once(atom) =>
      //TODO cache attribution
      val spd = new SuperPosed(new RelTree[Expression, Expression](root))
      (if(spd.isSuperPosed(atom)) ().successNel else "argument of `once` must be superposed".failureNel) *> super.doAnalyze(n)
    case n @ _ => super.doAnalyze(n)
  }
}

sealed trait SuperPosGenerators extends ListExtensions with Symbols {
  private sealed abstract class SuperPosed[T: ClassTag](implicit ev: ArgType[T]) {
    def toTree: NondetGenerator
    def tpe = ev
  }

  private case class SuperPosRange[T: ClassTag](from: T, to: T)(implicit typ: ScalaType[T]) extends SuperPosed[T] {
    def toTree = NondetGeneratorRange(Literal(from), Literal(to)) // TODO: źle - to musi zostac nazwane.
  }

  private case class SuperPosChoice[T: ClassTag](choice: Rep[ArgType[List[T]]])(implicit typ: ArgType[T]) extends SuperPosed[T] {
    def toTree = NondetGeneratorList(choice.tree)
  }
  private object SuperPosChoice {
    def apply[T: ClassTag](choice: Rep[List[T]])(implicit ev: ArgType[T]) : SuperPosed[T] = {
      new SuperPosed[T] {
        override def toTree = NondetGeneratorList(choice.tree)
      }
    }
  }

  private implicit def superPosedToRep[B1](sp: SuperPosed[B1])(implicit tpe: UnliftedArgType[B1]): Rep[B1] = {
    val t: Atom = sp.toTree(sp.tpe)
    new Rep[B1] {
      override val tree: Expression = t
    }
  }

  object choose extends ToTypedTreeOps {
    abstract class Between[T: ClassTag] {
      val from: T
      def and(t: T)(implicit typ: ScalaType[T]): Rep[T] = SuperPosRange(from, t)
    }
    def between[T: ClassTag](frm: T) = new Between[T] { val from = frm }
    def from[T : UnliftedArgType : ClassTag](from: Rep[List[T]]): Rep[T] = SuperPosChoice(from) //FIXME: should accept superposed types
  }
}

private[extensions] class SuperPosMapper[B1, B2, BR] {
  import AST.syntax._
  def toRep(name: Expression, args: Expression*)(implicit tpe: ArgType[BR]): Rep[BR] = {
    new Rep[BR] {
      override val tree: Expression = Apply(name, args.toList)(tpe)
    }
  }
}

trait SuperPosContCompiler extends BaseContCompiler with SuperPosExtensions with SuperPosAnalysis {
  override def eval(expr: Expression, cont: (Any) => Cont): Cont = expr match {
    case sym @ Expr(c:NondetGenerator) => (s: State) => cont(s(sym.asInstanceOf[Sym].name))(s)

    case Once(sym:Sym) => // in the future this will be simply desugared
      val space = showSpace(sym)
      val states: Iterable[Cont] = space map (kv => kv._2 match {
        case ListConstructor(x) => eval(x.head, v => (s: State) => s + (kv._1 -> v))
        case Literal(x: List[Int]) => (s: State) => s + (kv._1 -> x)
        case (left:Int, _) => (s: State) => s + (kv._1 -> left)
      })
      val contWithState: Cont = Function chain states.toSeq
      contWithState andThen eval(sym, (x) => cont(x))

    case _ => super.eval(expr, cont)
  }
}
