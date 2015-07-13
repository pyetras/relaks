package relaks.lang.dsl

import com.bethecoder.ascii_table.ASCIITable
import org.kiama.relation.GraphTree
import relaks.lang.ast.{Literal, Expression}
import relaks.lang.dsl.extensions._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.impl.Row
import relaks.lang.impl
import relaks.optimizer.GridOptimizer

import scalaz.concurrent.Task
import scalaz.stream._

/**
 * Created by Pietras on 26/06/15.
 */
trait Interpreter
  extends Environments
  with SuperPosAnalysis
  with SuperPosInterpreter
  with QueryInterpreter
  with GridOptimizer
  with TableIO
  with TableCompilerPhases
  with TupleInterpreter {
  def eval(expr: Expression): Process[Task, impl.Row] = expr match {
    case _/>(c @ Comprehension(_/>OptimizerResultTable(vars), transforms, filters, limits, orderbys, groupbys)) =>

      val superPosed = new SuperPosed(new GraphTree(vars))
      val paramSources = superPosed.superPosDeps(vars)
      val params = paramSources.toSeq.map(sym => sym -> evalSuperPosGenerator(sym))
      val paramsSpace = collection.mutable.LinkedHashMap(params.map(symParam => s"x${symParam._1.name}" -> symParam._2):_*)

      val optimizer = new GrOptimizer(paramsSpace, StrategyMinimize)

      //find value to optimize on
      val Some(outputSchema) = OutputSchema.forComprehension(c)
      val (orderby: OrderBy) +: Nil = orderbys
      val FieldWithDirection(name, Asc) = orderby.ordering.head

      val toMinimizeIx = outputSchema.map(_._1).indexOf(name.name)
      assert(toMinimizeIx >= 0, "Invalid optimization condition")

      val (transform: Transform) +: Nil = transforms

      val loop: Process1[Params, (impl.Row, OptimizerResult)] = process1.lift { params =>
        push(params.map(nameVal => Sym(nameVal._1.drop(1).toInt) -> new Literal(nameVal._2))) //TODO types?
        //evaluate the input tuple to row
        val inputRow: impl.Row = evalTupleExpression(vars)

        val resultRow = evalQuery(inputRow, transform)

        (resultRow, OptimizerResult(resultRow(toMinimizeIx), params))
      }

      def fst[L]: Process1[(L, _), L] = process1.lift {_._1}

      //todo filter
      optimizer.paramStream
        .pipe(loop)
        .observe(optimizer.update.contramap(_._2)) //pipe second element (OptimizerResult) to update sink
        .pipe(fst)
  }

  def dump(): Unit = {
    for (expr <- storedOutput) {
      val stream = eval(buildComprehensions(expr).get)
      val rows = stream.runLog.run
      if (rows.nonEmpty) {
        ASCIITable.getInstance().printTable(rows.head.colNames.toArray, rows.map(_.values.map(_.toString).toArray).toArray)
      }
    }
  }
}

trait Environments extends Symbols {
  var history = List.empty[Map[Sym, Expression]]
  var cachedDefinitions = Map.empty[Sym, Expression]

  private[dsl] def push(update: Iterable[(Sym, Expression)] = Map.empty) = {
    history = cachedDefinitions :: history
    cachedDefinitions ++= update
  }
  private[dsl] def pop() = {
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

trait BaseQueryInterpreter extends Environments {
  def evalQuery(inputRow: Row, q: Query): Row = throw new NotImplementedError(s"Evaluating query $q not implemented")
}