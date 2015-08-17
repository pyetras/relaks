package relaks.lang.phases.interpreter

import relaks.lang.ast.{Pure, Expression, Literal}
import relaks.lang.dsl.extensions.ast.Queries
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.impl.Row

import scalaz.stream.{process1, Process1}

/**
 * Created by Pietras on 12.08.15.
 */
trait QueryOpInterpreter extends BaseQueryOpInterpreter with Queries with BaseExprInterpreter {
  import QueryOp._

  private def inEnv[T](gen: Generator)(f: => T)(inputRow: Row) = {
    push(gen.symsToFields
      .map {case (sym, name) => (sym, new Literal(inputRow(name.name))(sym.tpe)) })
    val result = f
    pop()
    result
  }

  override def evalQuery(q: QueryOp): Process1[Row, Row] = q match {
    case Transform(gen: Generator, select) => //TODO dodac cache dla row schema do wierzcholka Transform
      process1.lift(inEnv(gen) {
          evalExpression(select).asInstanceOf[Row]
        })

    case Filter(gen: Generator, filter) =>
      process1.filter(inEnv(gen) {
        evalExpression(filter).asInstanceOf[Boolean]
      })

    case Limit(startExpr, countExpr) =>
      val start = evalExpression(startExpr).asInstanceOf[Int]
      val count = evalExpression(countExpr).asInstanceOf[Int]
      process1.drop[Row](start).take(count)

    case _ => super.evalQuery(q)
  }

  override def evalExpression(expr: Expression): Any = expr match {
    case _/>Pure(e) => evalExpression(e)
    case _ => super.evalExpression(expr)
  }
}
