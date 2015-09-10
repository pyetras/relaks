package relaks.lang.phases.interpreter

import com.bethecoder.ascii_table.ASCIITable
import com.typesafe.scalalogging.LazyLogging
import org.kiama.relation.GraphTree
import relaks.lang.ast._
import relaks.lang.dsl.extensions._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.ast.logical.{LoadComprehension, QueryOp, SelectComprehension}
import relaks.lang.impl
import relaks.lang.ast
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

abstract class OptimizationInterpreter(Optimizer: BaseOptimizer)
  extends ComprehensionInterpreter
  with NondetParams
  with HyperparamAnalysis
  with HyperparamInterpreter
  with LazyLogging {
  private val evalOptimizerComprehension: PartialFunction[Expression, Process[Task, impl.Row]] = {
    case _/>(c @ SelectComprehension(input@LoadComprehension(OptimizerResultTable(vars)), transforms, filters, limits, orderbys, sequence)) =>
      import QueryOp._

      val superPosed = new SuperPosed(new GraphTree(vars))
      val paramSources = superPosed.superPosDeps(vars)
      val params = paramSources.toSeq.map(sym => sym -> evalSuperPosGenerator(sym))
      val paramsSpace = collection.mutable.LinkedHashMap(params.map(symParam => s"x${symParam._1.name}" -> symParam._2):_*)

      val optimizer = Optimizer(1, paramsSpace, Optimizer.StrategyMinimize)
      logger.debug(optimizer.toString)
      val generate: Process1[Params, (impl.Row, Params)] = process1.lift { params =>
        push(params.map(nameVal => Sym(nameVal._1.drop(1).toInt) -> new Literal(nameVal._2))) //TODO types?
        //evaluate the input tuple to row
        val inputRow: impl.Row = evalTupleExpression(vars)
        pop()
        (inputRow, params)
      }

      def test(toMinimizeIx: Int): Process1[(impl.Row, Params), (impl.Row, Optimizer.OptimizerResult)] = process1.lift { in =>
        val (row, params) = in
        (row, Optimizer.OptimizerResult(row(toMinimizeIx), params))
      }

      def fst[L]: Process1[(L, _), L] = process1.lift {_._1}

      def chainWithP(seq: Seq[QueryOp],
                     acc: Process[Task, (impl.Row, Params)],
                     lastTransform: Option[Transform] = None): Trampoline[Process[Task, impl.Row]] =
        seq match {
          case Seq() => null
          case op +: rest =>
            def nextPipe(op: QueryOp, transOpt: Option[Transform] = lastTransform) =
              Trampoline.suspend(chainWithP(rest, acc |> process1.liftFirst((x: impl.Row) => none[Params])(evalQuery(op)), transOpt))
            op match {
              case OrderBy(ordering, true) =>
                //find value to optimize on
                val outputSchema = lastTransform.map(OutputSchema.forTransform).getOrElse(OutputSchema.forComprehension(input))
                val FieldWithDirection(Field(name, _), ast.OrderBy.Asc) = ordering.head

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

  override protected[lang] def evalComprehensionPartial = evalOptimizerComprehension orElse super.evalComprehensionPartial
}
