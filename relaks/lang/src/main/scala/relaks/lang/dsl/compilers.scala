package relaks.lang.dsl

import org.kiama.attribution.Attribution._
import org.kiama.rewriting.Rewriter
import relaks.lang.ast._
import relaks.lang.dsl.extensions._

import scalaz.Scalaz._
import scalaz.{Scalaz, ValidationNel}

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseCompiler {
  type Result
  final def analyze(root: Expression) = {
    initTree(root)
    val strategy = Rewriter.collect[List, ValidationNel[String, Unit]] ({
      case (expr:Expression) => doAnalyze(expr)
    })

    strategy(root).reduce { _ *> _}
  }
  protected def doAnalyze(root: Expression): ValidationNel[String, Unit] =
    ().successNel[String]

  def compile(expr: Expression): Result
  def run(res: Result): Any
}

trait BaseContCompiler extends BaseCompiler with Symbols {
  type State = Map[Int, Any]
  type Cont = State => State

  override type Result = Cont

  def eval(expr: Expression, cont: Any => Cont): Cont = expr match {
    case Expr(Literal(v)) => cont(v)
  }

  override def compile(expr: Expression): Cont = {
    initTree(expr)
    val cont: Any => Cont = (x:Any) => (s:State) => s + (-1 -> x)
    eval(expr, cont)
  }

  override def run(cont: Cont): Any = {
    val init = Map[Int, Any]()
    val mem = cont(init)
    mem(-1)
  }
}

trait ContCompiler extends BaseContCompiler with NumericContCompiler with SuperPosContCompiler