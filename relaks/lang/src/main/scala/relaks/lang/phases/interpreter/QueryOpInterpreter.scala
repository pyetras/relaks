package relaks.lang.phases.interpreter

import com.twitter.algebird.mutable.InfPriorityQueueAggregator
import relaks.lang.ast._
import relaks.lang.dsl.extensions.ast.Queries
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.impl.Row
import relaks.lang.ast

import scalaz.{Ordering, Order, Scalaz}
import Scalaz._
import scalaz.stream._

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

    case OrderBy(fields, false) =>
      def mkOrder(fld: FieldWithDirection): Order[Row] = {
        val order = new Order[Row] {
          override def order(x: Row, y: Row): Ordering = {
            val ord = fld.field.typ.asInstanceOf[ArgType[Any]].order
            ord(x(fld.field.sym.name), y(fld.field.sym.name))
          }
        }
        (fld.direction == ast.OrderBy.Desc) ? order | order.reverseOrder  //order is reversed again in the aggregation
      }
      implicit val ordering: scala.Ordering[Row] = fields.toEphemeralStream.map(mkOrder).fold.toScalaOrdering
      val aggregator = new InfPriorityQueueAggregator[Row]

      process1.lift(aggregator.prepare) |>
        process1.reduce(aggregator.reduce) |>
        process1.lift(aggregator.present) flatMap { (x: Seq[Row]) =>
        Process.emitAll(x)
      }

    case _ => super.evalQuery(q)
  }

  override def evalExpression(expr: Expression): Any = expr match {
    case _/>Pure(e) => evalExpression(e)
    case _ => super.evalExpression(expr)
  }
}
