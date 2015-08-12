package relaks.lang.dsl.extensions.ast.logical

/**
 * Created by Pietras on 12.08.15.
 */

import org.kiama.output.PrettyPrinterTypes.{Document, Width}
import relaks.lang.ast.{Atom, Expression}
import relaks.lang.dsl.extensions.ast
import relaks.lang.dsl.extensions.ast.{FieldWithDirection, SourceQuery, GeneratorBase}
import scalaz.Scalaz
import Scalaz._
import org.kiama.output.PrettyPrinter

import scala.runtime.ScalaRunTime

/**
 * Created by Pietras on 12.08.15.
 */
sealed trait Comprehension extends Expression
sealed case class LoadComprehension(loadExpression: SourceQuery) extends Comprehension

object ComprehensionPrinter extends PrettyPrinter {

  def apply(comprehension: Comprehension): String = pretty(cmpToDoc(comprehension)).layout

  private def nonEmpty(s: Seq[_])(f: => Doc) = if (s.isEmpty) empty else f

  private def genToDoc(gen: GeneratorBase) = value(gen) <+> "→ "

  import QueryOp._
  private def opToDoc(queryOp: QueryOp.QueryOp) = queryOp match {
    case (op: Filter) => genToDoc(op.generator) <> value(op.filter)
    case (op: Transform) => genToDoc(op.generator) <> value(op.select)
    case (op: OrderBy) => list(op.ordering.toList, prefix = "")
    case _ => value(queryOp)
  }

  private def cmpToDoc(comprehension: Comprehension): Doc = comprehension match {
    case SelectComprehension(from, transform, filter, limit, orderBy, seq) =>
      group("from" <@> nest(cmpToDoc(from))) <@>
      group("select" <@> nest(group(vsep(transform.map(opToDoc))))) <>
      nonEmpty(filter)(line <> group("where" <@> nest(group(vsep(filter.map(opToDoc)))))) <>
      nonEmpty(orderBy)(line <> group("order by" <@> nest(group(vsep(orderBy.map(opToDoc)))))) <>
      nonEmpty(limit)(line <> group("limit" <@> nest(group(vsep(limit.map(opToDoc))))))
    case LoadComprehension(x) => value(x)
  }
}

final case class SelectComprehension(from: Comprehension,
                                transform: Vector[QueryOp.Transform] = Vector.empty,
                                filter: Vector[QueryOp.Filter] = Vector.empty,
                                limit: Vector[QueryOp.Limit] = Vector.empty,
                                orderBy: Vector[QueryOp.OrderBy] = Vector.empty,
                                sequence: Vector[QueryOp.QueryOp] = Vector.empty) extends Comprehension {

  import QueryOp._
  def append(queryOp: QueryOp): SelectComprehension = queryOp match {
    case (op: Filter) => this.copy(filter = this.filter :+ op, sequence = this.sequence :+ op)
    case (op: Transform) => this.copy(transform = this.transform :+ op, sequence = this.sequence :+ op)
    case (op: Limit) => this.copy(limit = this.limit :+ op, sequence = this.sequence :+ op)
    case (op: OrderBy) => this.copy(orderBy = this.orderBy :+ op, sequence = this.sequence :+ op)
  }

  def appendAndCommit(queryOp: QueryOp): SelectComprehension = SelectComprehension(append(queryOp))

  override def mainToString: String = ScalaRunTime._toString(this)

}

object QueryOp {
  sealed trait QueryOp

  case class Transform(generator: GeneratorBase, select: Atom) extends QueryOp
  case class Filter(generator: GeneratorBase, filter: Atom) extends QueryOp
  case class Limit(start: Atom, count: Atom) extends QueryOp
  case class OrderBy(ordering: Vector[FieldWithDirection]) extends QueryOp

  def unapply(expr: Expression): Option[QueryOp] = expr match {
    case (op: ast.Filter) => Filter(op.generator, op.filter).some
    case (op: ast.Transform) => Transform(op.generator, op.select).some
    case (op: ast.Limit) => Limit(op.start, op.count).some
    case (op: ast.OrderBy) => OrderBy(op.ordering).some

  }
}
