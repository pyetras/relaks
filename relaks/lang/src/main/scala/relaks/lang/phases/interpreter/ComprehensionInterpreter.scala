package relaks.lang.phases.interpreter

import com.bethecoder.ascii_table.ASCIITable
import relaks.lang.ast.{Literal, Expression}
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.dsl.extensions.ast.logical.QueryOp
import relaks.lang.dsl.extensions.{TupleInterpreter, TableIO}
import relaks.lang.phases.rewriting.QueryRewritingPhases
import relaks.lang.impl

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._

/**
 * Created by Pietras on 26/06/15.
 */

trait ComprehensionInterpreter
  extends Environments
  with BaseExprInterpreter
  with BaseQueryOpInterpreter
  with QueryRewritingPhases
  with TupleInterpreter
  with TableIO {

  protected[lang] def evalComprehension(expr: Expression): Process[Task, impl.Row] =
    throw new NotImplementedError(s"Evaluating $expr not implemented")

  def run(expr: Expression) = {
    val stream = evalComprehension(buildComprehensions(expr).get)
    val rowsWithError = stream
      .observe(sink.lift(r => Task.now(logger.info(s"Got row: $r"))))
      .map((r: impl.Row) => scala.List(r)).scanMonoid //collect partial results in a grid
      .attempt[Task, Throwable]() //emit an error
      .sliding(2).map { case fst +: snd +: Seq() => (snd getOrElse fst.getOrElse(scala.List.empty[impl.Row]), snd.swap.toOption)}
    rowsWithError.runLast.run
  }

  def show(rows: Traversable[impl.Row], error: Option[Throwable]) = {
    if (rows.nonEmpty) {
      ASCIITable.getInstance().printTable(rows.head.colNames.toArray, rows.map(_.values.map(_.toString).toArray).toArray)
    }
    error foreach println
  }

  def dump(): Unit = {
    for (expr <- storedOutput) {
      val (rows, error) = run(expr).get
      show(rows, error)
    }
  }
}

trait Environments extends Symbols {
  var history = List.empty[Map[Sym, Expression]]
  var cachedDefinitions = Map.empty[Sym, Expression]

  private[interpreter] def push(update: Iterable[(Sym, Expression)] = Map.empty) = {
    history = cachedDefinitions :: history
    cachedDefinitions ++= update
  }
  private[interpreter] def pop() = {
    cachedDefinitions = history.head
    history = history.tail
  }
  override protected def findDefinition(sym: Sym): Option[Expression] = cachedDefinitions.get(sym).orElse(super.findDefinition(sym))

  protected def cache(sym: Sym, expr: Expression) = cachedDefinitions += (sym -> expr)
}

trait BaseExprInterpreter extends Environments with Symbols {
  def evalExpression(expr: Expression): Any = expr match {
    case _/>Literal(v) => v
    case _ => throw new NotImplementedError(s"Evaluating $expr not implemented")
  }
}

trait BaseQueryOpInterpreter extends Environments {
  import QueryOp._
  def evalQuery(q: QueryOp): Process1[impl.Row, impl.Row] = throw new NotImplementedError(s"Evaluating query $q not implemented")
}