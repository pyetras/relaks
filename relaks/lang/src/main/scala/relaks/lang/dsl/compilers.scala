package relaks.lang.dsl

import relaks.lang.dsl.AST._
import relaks.lang.dsl.extensions._
import org.kiama.attribution.Attribution._

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseCompiler {
  type Result
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