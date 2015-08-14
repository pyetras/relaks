package relaks.lang.phases.interpreter

import relaks.lang.ast.{Pure, Expression, Literal}
import relaks.lang.dsl.extensions.ast.Queries
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.impl.Row

import scalaz.stream.{process1, Process1}

/**
 * Created by Pietras on 12.08.15.
 */
trait QueryInterpreter extends BaseQueryInterpreter with Queries with BaseExprInterpreter {
  import QueryOp._
  override def evalQuery(q: QueryOp): Process1[Row, Row] = q match {
    case Transform(gen: Generator, select) =>
      process1.lift { inputRow =>
        push(gen.symsVector.zip(inputRow.values.map(new Literal(_))))
        evalExpression(select).asInstanceOf[Row]
      }
    case _ => super.evalQuery(q)
  }

  override def evalExpression(expr: Expression): Any = expr match {
    case _/>Pure(e) => evalExpression(e)
    case _ => super.evalExpression(expr)
  }
}
