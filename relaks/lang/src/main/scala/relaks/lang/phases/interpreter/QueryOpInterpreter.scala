package relaks.lang.phases.interpreter

import com.twitter.algebird.Averager
import com.twitter.algebird.mutable.UnboundedPriorityQueueAggregator
import org.kiama.==>
import relaks.lang.ast._
import relaks.lang.dsl.ScalaTypeImplis
import relaks.lang.dsl.extensions.ast.{Queries}
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.impl.Row
import relaks.lang.ast

import scalaz.{Ordering, Order, Scalaz}
import Scalaz._
import scalaz.stream._

/**
 * Created by Pietras on 12.08.15.
 */
trait QueryOpInterpreter extends BaseQueryOpInterpreter with Queries with BaseExprInterpreter with ComprehensionInterpreter with ScalaTypeImplis {
  import QueryOp._

  private def inEnv[T](gen: Generator)(f: => T)(inputRow: Row) = {
    push(gen.symsToFields
      .map {case (sym, name) => (sym, new Literal(inputRow.get(name.name)(sym.tpe.asInstanceOf[ArgType[Any]]))(sym.tpe)) })
    val result = f
    pop()
    result
  }

  override def evalQuery(q: QueryOp): Process1[Row, Row] = q match {
    case Transform(gen: EmptyGenerator, select) =>
      process1.lift { (row: Row) =>
        push(List(gen.row -> new Literal(row)))
        val result = evalExpression(select).asInstanceOf[Row]
        pop()
        result
      }

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
      val aggregator = new UnboundedPriorityQueueAggregator[Row]

      process1.lift(aggregator.prepare) |>
        process1.reduce(aggregator.reduce) |>
        process1.lift(aggregator.present) flatMap { (x: Seq[Row]) =>
        Process.emitAll(x)
      }

    case _ => super.evalQuery(q)
  }

  def evalAggregator: Expression ==> Any = {
    case _/> Aggregate(Aggregator.Avg, cmp) =>
      val stream = evalComprehension(cmp)
      val aggregator = Averager
      val process = stream |> process1.lift(row => row.get[Double](0)) |>
      process1.lift(aggregator.prepare) |>
      process1.reduce(aggregator.reduce) |>
      process1.lift(aggregator.present)
      process.runLast.run.orNull
  }

  override def evalExpression(expr: Expression): Any = expr match {
    case _/>Pure(e) => evalExpression(e)
    case _ => evalAggregator.applyOrElse(expr, super.evalExpression)
  }
}
