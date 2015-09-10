package relaks.lang.dsl.extensions

import org.kiama.attribution.Attribution
import org.kiama.relation.GraphTree
import relaks.lang.ast._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.phases.analysis.Analysis
import relaks.lang.phases.interpreter.{BaseExprInterpreter, Environments}
import relaks.optimizer.{ChooseOneOf, NondetParam, DiscreteRange, ContinuousRange}
import shapeless.HList
import shapeless.ops.hlist

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.Scalaz._
import scalaz.{Scalaz, ValidationNel}
import relaks.lang.ast.ScalaType

/**
 * Created by Pietras on 13/04/15.
 */
trait HyperparamExtensions extends ListExtensions with Symbols with HyperparamGenerators with TableOps {

  def once[T](superPos: Rep[T]): Rep[T] = {
    new Rep[T] {
      override val tree: Expression = Once(superPos.tree)(superPos.getTpe)
    }
  }

  def optimize[H <: HList, Out](varTup: Rep[Tup[H]])
                               (implicit mkCmp: BuildComprehension[Rep[UnfinishedGenTable[Tup[H]]], Out]) = {
    val expr = OptimizerResultTable(varTup.tree)
    mkCmp(expr)
  }
}

trait HyperparamAnalysis extends Symbols with Analysis {

  class SuperPosed(tree: GraphTree) extends Attribution { self =>
    val superPosDeps: Any => Set[Sym] = {
      attr {
        case _ /> OptimizerResultTable(_) => Set.empty
        case Some(sym) /> (_: HyperparamSpace) => Set(sym)
        case Fresh(_) => Set.empty[Sym]
        case _ /> (link) => tree.child(link).map(self.superPosDeps).foldLeft(Set.empty[Sym]){_ ++ _}
        case _ => Set.empty
      }
    }

    def isSuperPosed(expr: Expression) = self.superPosDeps(expr).nonEmpty
  }

  //TODO attribution
  def showSpace(superPos: Expression): Map[Int, Any] = ??? /*{
    assert(superPos->isSuperPosed)
    (superPos->superPosDeps map {
      case sym @ Expr(HyperparamList(Expr(lc @ ListConstructor(lst)))) => sym.asInstanceOf[Sym].name -> lc
      case sym @ Expr(HyperparamList(Expr(ll @ Literal(x)))) => sym.asInstanceOf[Sym].name -> ll
      case sym @ Expr(HyperparamRange(l, r)) => sym.asInstanceOf[Sym].name -> (l.value, r.value)
    }).toMap
  }*/

  override protected def doAnalyze(root: Expression): ValidationNel[String, Unit] = root match {
    case n @ Once(atom) =>
      //TODO cache attribution
      val spd = new SuperPosed(new GraphTree(root))
      (if(spd.isSuperPosed(atom)) ().successNel else "argument of `once` must be superposed".failureNel) *> super.doAnalyze(n)
    case n @ _ => super.doAnalyze(n)
  }
}

sealed trait HyperparamGenerators extends ListExtensions with Symbols {
  sealed trait HasRange[T]
  object HasRange {
    implicit val intHasRange = new HasRange[Int] {}
    implicit val doubleHasRange = new HasRange[Double] {}
  }

  private def mkHyperparamRep[T: ArgType](paramExpr: Expression): Rep[T] = new Rep[T] {
    override val tree: Atom = paramExpr(implicitly[ArgType[T]])
  }

  private def superPosChoice[T: ArgType](choice: Rep[List[T]]) =
    mkHyperparamRep[T](HyperparamList(choice.tree))

  private def superPosRange[T: ArgType: HasRange](from: Rep[T], to: Rep[T]) =
    mkHyperparamRep[T](HyperparamRange(from.tree, to.tree))

  object choose extends ToTypedTreeOps {
    abstract class Between[T: ArgType: HasRange] {
      val from: Rep[T]
      def and(t: Rep[T]): Rep[T] = superPosRange(from, t)
    }
    def between[T: ArgType: HasRange](frm: Rep[T]) = new Between[T] { val from = frm }
    def from[T : ArgType](from: Rep[List[T]]): Rep[T] = superPosChoice(from)
  }
}

trait HyperparamContCompiler extends BaseContCompiler with HyperparamExtensions with HyperparamAnalysis with Environments {
  override def eval(expr: Expression, cont: (Any) => Cont): Cont = expr match {
    case sym @ Expr(c:HyperparamSpace) => (s: State) => cont(s(sym.asInstanceOf[Sym].name))(s)

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