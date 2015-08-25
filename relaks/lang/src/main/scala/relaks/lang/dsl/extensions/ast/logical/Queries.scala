package relaks.lang.dsl.extensions.ast.logical

/**
 * Created by Pietras on 12.08.15.
 */

import relaks.lang
import relaks.lang.ast._
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.dsl.extensions.ast.logical.QueryOp.QueryOp
import relaks.lang.dsl.extensions.ast.{Queries, Symbols}
import scalaz._
import Scalaz._
import org.kiama.output.PrettyPrinter

import scala.runtime.ScalaRunTime

/**
 * Created by Pietras on 12.08.15.
 */

sealed trait Comprehension extends Expression
sealed case class LoadComprehension(loadExpression: SourceQuery) extends Comprehension

final case class SelectComprehension(from: Comprehension,
                                     transform: Vector[QueryOp.Transform] = Vector.empty,
                                     filter: Vector[QueryOp.Filter] = Vector.empty,
                                     limit: Vector[QueryOp.Limit] = Vector.empty,
                                     orderBy: Vector[QueryOp.OrderBy] = Vector.empty,
                                     sequence: Vector[QueryOp] = Vector.empty) extends Comprehension {
  import QueryOp._

//  val seqL = Lens.lensu[SelectComprehension, QueryOp]((cmp, ops) => cmp.copy(sequence = cmp.sequence :+ ops), _.sequence.first)
//  val filterL = Lens.lensu[SelectComprehension, Filter]((cmp, ops) => cmp.copy(filter = ops), _.filter)

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
  sealed trait QueryOp extends Expression

  case class Transform(generator: GeneratorBase, select: Atom) extends QueryOp
  case class Filter(generator: GeneratorBase, filter: Atom) extends QueryOp
  case class Limit(start: Atom, count: Atom) extends QueryOp
  case class OrderBy(ordering: Vector[FieldWithDirection], isExperimentTarget: Boolean) extends QueryOp

  def unapply(expr: Expression): Option[QueryOp] = expr match {
    case (op: lang.ast.Filter) => Filter(op.generator, op.filter).some
    case (op: lang.ast.Transform) => Transform(op.generator, op.select).some
    case (op: lang.ast.Limit) => Limit(op.start, op.count).some
    case (op: lang.ast.OrderBy) => OrderBy(op.ordering, op.isExperimentTarget).some
  }

  private object Inner {
    def unapply(queryOp: QueryOp) = queryOp match {
      case Transform(_, select) => select.some
      case Filter(_, filter) => filter.some
      case _ => None
    }
  }
}

trait ComprehensionPrinters extends Symbols with Queries {

  object ComprehensionPrinter extends PrettyPrinter {

    def apply(comprehension: Comprehension): String = pretty(cmpToDoc(comprehension)).layout

    private def nonEmpty(s: Seq[_])(f: => Doc) = if (s.isEmpty) empty else f

    private def genToDoc(gen: GeneratorBase) = arguments(gen.asInstanceOf[Generator].fields.toList) <+> "→"

    private def exprToDoc(expr: Atom) = expr match {
      case _/>Pure(_/>(t: TupleConstructor)) => arguments(t.names)
      case _/>(c: Comprehension) => cmpToDoc(c)
      case _ =>value(expr)
    }

    import QueryOp._

    private def opToDoc(queryOp: QueryOp.QueryOp) = queryOp match {
      case (op: Filter) => group(genToDoc(op.generator) <> nestl(exprToDoc(op.filter)))
      case (op: Transform) => group(genToDoc(op.generator) <> nestl(exprToDoc(op.select)))
      case (op: OrderBy) => arguments(op.ordering.toList)
      case _ => value(queryOp)
    }

    private def nestl(f: => Doc) = nest(line <> f)

    private def cmpToDoc(comprehension: Comprehension): Doc = comprehension match {
      case SelectComprehension(from, transform, filter, limit, orderBy, seq) =>
        group("from" <> nestl(cmpToDoc(from))) <@>
          group("select" <> nestl(group(vsep(transform.map(opToDoc))))) <>
          nonEmpty(filter)(line <> group("where" <> nestl(group(vsep(filter.map(opToDoc)))))) <>
          nonEmpty(orderBy)(line <> group("order by" <> nestl(group(vsep(orderBy.map(opToDoc)))))) <>
          nonEmpty(limit)(line <> group("limit" <> nestl(group(vsep(limit.map(opToDoc))))))
      case LoadComprehension(x) => value(x)
    }
  }

}
