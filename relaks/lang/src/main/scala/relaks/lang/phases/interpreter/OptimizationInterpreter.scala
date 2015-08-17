package relaks.lang.phases.interpreter

import com.bethecoder.ascii_table.ASCIITable
import org.kiama.relation.GraphTree
import relaks.lang.ast._
import relaks.lang.dsl.extensions._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.ast.logical.{LoadComprehension, QueryOp, SelectComprehension}
import relaks.lang.impl
import relaks.lang.impl.Row
import relaks.lang.phases.rewriting.QueryRewritingPhases
import relaks.optimizer.{NondetParams, NondetParam, BaseOptimizer, GridOptimizer}

import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz.Free.Trampoline
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{Trampoline, Scalaz}
import Scalaz._

/**
 * Created by Pietras on 26/06/15.
 */

abstract class OptimizationInterpreter(Optimizer: BaseOptimizer)
  extends Environments
  with NondetParams
  with SuperPosAnalysis
  with BaseExprInterpreter
  with SuperPosInterpreter
  with BaseQueryOpInterpreter
  with QueryRewritingPhases
  with TupleInterpreter {
  private def evalComprehension(expr: Expression): Process[Task, impl.Row] = expr match {
    case _/>(c @ SelectComprehension(LoadComprehension(OptimizerResultTable(vars)), transforms, filters, limits, orderbys, sequence)) =>
      import QueryOp._

      val superPosed = new SuperPosed(new GraphTree(vars))
      val paramSources = superPosed.superPosDeps(vars)
      val params = paramSources.toSeq.map(sym => sym -> evalSuperPosGenerator(sym))
      val paramsSpace = collection.mutable.LinkedHashMap(params.map(symParam => s"x${symParam._1.name}" -> symParam._2):_*)

      val optimizer = Optimizer(1, paramsSpace, Optimizer.StrategyMinimize)

      val generate: Process1[Params, (impl.Row, Params)] = process1.lift { params =>
        push(params.map(nameVal => Sym(nameVal._1.drop(1).toInt) -> new Literal(nameVal._2))) //TODO types?
        //evaluate the input tuple to row
        val inputRow: impl.Row = evalTupleExpression(vars)
        (inputRow, params)
      }

      def test(toMinimizeIx: Int): Process1[(impl.Row, Params), (impl.Row, Optimizer.OptimizerResult)] = process1.lift { in =>
        val (row, params) = in
        (row, Optimizer.OptimizerResult(row(toMinimizeIx), params))
      }

      def fst[L]: Process1[(L, _), L] = process1.lift {_._1}

      def chainWithP(seq: List[QueryOp],
                     acc: Process[Task, (impl.Row, Params)],
                     lastTransform: Option[Transform] = None): Trampoline[Process[Task, impl.Row]] =
        seq match {
          case Nil => null
          case op :: rest =>
            def nextPipe(op: QueryOp, transOpt: Option[Transform] = lastTransform) =
              Trampoline.suspend(chainWithP(rest, acc |> process1.liftFirst((x: impl.Row) => none[Params])(evalQuery(op)), transOpt))
            op match {
              case OrderBy(ordering, true) =>
                //find value to optimize on
                val outputSchema = OutputSchema.forTransform(lastTransform.get)
                val FieldWithDirection(name, GroupBy.Asc) = ordering.head

                val toMinimizeIx = outputSchema.map(_._1).indexOf(name.name)
                assert(toMinimizeIx >= 0, "Invalid optimization condition")

                Trampoline.done {
                  rest.foldLeft((acc |> test(toMinimizeIx)).observe(optimizer.update.contramap(_._2)) |> fst) { _ |> evalQuery(_) }
                }
              case t: Transform => nextPipe(op, t.some)
              case _ => nextPipe(op)
            }
        }


      chainWithP(sequence, optimizer.paramStream |> generate).run
  }

  def run(expr: Expression) = {
    val stream = evalComprehension(buildComprehensions(expr).get)
    val rowsWithError = stream
      .observe(sink.lift(r => Task.now(println(r)))) //output partial results
      .map((r: impl.Row) => scala.List(r)).scanMonoid //collect partial results in a grid
      .attempt[Task, Throwable]() //emit an error
      .sliding(2).map { case fst +: snd +: Seq() => (snd getOrElse fst.getOrElse(scala.List.empty[impl.Row]), snd.swap.toOption)}
    rowsWithError.runLast.run
  }


  def dump(): Unit = {
    for (expr <- storedOutput) {
      val (rows, error) = run(expr).get
      if (rows.nonEmpty) {
        ASCIITable.getInstance().printTable(rows.head.colNames.toArray, rows.map(_.values.map(_.toString).toArray).toArray)
      }
      error foreach println
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