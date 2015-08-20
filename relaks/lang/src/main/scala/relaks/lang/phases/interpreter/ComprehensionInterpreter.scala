package relaks.lang.phases.interpreter

import com.bethecoder.ascii_table.ASCIITable
import org.kiama.==>
import relaks.lang.ast.{ListType, TableFromList, Literal, Expression}
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.dsl.extensions.ast.logical.{LoadComprehension, SelectComprehension, QueryOp}
import relaks.lang.dsl.extensions.{ListInterpreter, ListExtensions, TupleInterpreter, TableIO}
import relaks.lang.impl.{TableImpl, Row}
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

  protected[lang] def evalComprehensionPartial: PartialFunction[Expression, Process[Task, impl.Row]] = PartialFunction.empty

  protected[lang] final def evalComprehension(expr: Expression): Process[Task, impl.Row] =
    evalComprehensionPartial applyOrElse(expr, (x: Expression) => throw new NotImplementedError(s"Evaluating $expr not implemented"))

  def run(expr: Expression) = {
    val stream = evalComprehension(buildComprehensions(expr).get)
    val rowsWithError = stream
      .observe(sink.lift(r => Task.now(logger.info(s"Got row: $r"))))
      .map((r: impl.Row) => scala.List(r)).scanMonoid //collect partial results in a grid
      .attempt[Task, Throwable]() //emit an error
      .zipWithPrevious.collect {
        case (_, \/-(rows)) => (rows, none[Throwable])
        case (Some(\/-(rows)), -\/(throwable)) => (rows, throwable.some)
        case (None, -\/(throwable)) => (List.empty[Row], throwable.some)
    }
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

  protected val operatorsProcess: PartialFunction[Expression, Process1[impl.Row, impl.Row]] = {
    case _/> SelectComprehension(_, _, _, _, _, seq) =>
      seq.foldl(process1.id[impl.Row])(acc => op => acc |> evalQuery(op))
  }

  protected def generatorPlusOps(generatorProc: Expression ==> Process[Task, Row]) = {
    val id = implicitly[Arrow[PartialFunction]].id[Expression]
    (id &&& id) >>> generatorProc *** operatorsProcess >>> { case (generator, ops) => generator |> ops }
  }


  override def evalExpression(expr: Expression): Any =
    evalComprehensionPartial.andThen(proc => new TableImpl(proc)).applyOrElse(expr, super.evalExpression)
}

trait ListComprehensionInterpreter extends ComprehensionInterpreter with ListInterpreter {
  private def rowsFromList: Expression ==> Process[Task, Row] = {
    case _/>SelectComprehension(LoadComprehension(TableFromList(listExpr)), _ +: _, _, _, _, _) =>
      val lst = evalListExpression(listExpr)
      val schema = Vector(("x0", listExpr.tpe.asInstanceOf[ListType[Any]].childType))

      //TODO rewrite this as an agent? writer?
      lst.foldRight(Process.empty[Task, Any])(x => acc => Process.emit(x) ++ acc) |> process1.lift { x =>
        new Row(Vector(x), schema)
      }
  }

  override protected[lang] def evalComprehensionPartial = generatorPlusOps(rowsFromList) orElse super.evalComprehensionPartial
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