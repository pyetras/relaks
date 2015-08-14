package relaks.optimizer

import relaks.data.LabelledTuples

import shapeless.ops.record.Selector
import shapeless.{Witness, HList}
import scalaz._
import Scalaz._
import scalaz.concurrent._
import scalaz.stream._

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag
import scala.util.DynamicVariable
import scala.language.implicitConversions

/**
 * Created by Pietras on 23/04/15.
 */

trait Experiments extends NondetParams with BaseOptimizer with LabelledTuples {
  import util._

  implicit val currentValueStore = new DynamicVariable(Map[String, Any]())
  implicit def extract[T: TypeTag](from: Nondet): T = currentValueStore.value(from.name).asInstanceOf[T]

  implicit class NondetOps(self: Nondet) {
    def as[T: TypeTag]: T = extract(self)
  }

  /**
   * Experiment description
   *
   * @param expr the experiment expression, evaluates to a LabelledTuple result
   * @param space parameter space description
   * @param getObjective getter for a cost value (objective) from the LabelledTuple result
   * @param strategy currently no op
   * @tparam R LabelledTuple inner type (shapeless record)
   * @tparam O objective type
   */
  class Experiment[R <: HList, O](expr: () => LabelledTuple[R], space: ParamsSpace, getObjective: LabelledTupleSelector[R, O], strategy: ExperimentStrategy) {
    def run(): Process[Task, LabelledTuple[R]] = {
      val optimizer = apply(1, space, strategy)

      val loop: Process1[Params, (LabelledTuple[R], OptimizerResult)] = process1.lift { params =>
        val resultTuple = currentValueStore.withValue(params) {
          expr() // mozna pozbierac ktore zmienne sa tak faktycznie wywolywane...
        }
        (resultTuple, OptimizerResult(getObjective(resultTuple), params))
      }

      def fst[L]: Process1[(L, _), L] = process1.lift {_._1}
//      def snd[R]: Process1[(_, R), R] = process1.lift {_._2}

      lazy val parallelEval: Process[Task, (LabelledTuple[R], OptimizerResult)] =
        nondeterminism.njoin(0, 0){ optimizer.paramStream.map(params => Process.eval(Task.delay(params)) |> loop )}

      optimizer.paramStream.pipe(loop)
//      parallelEval
        .observe(optimizer.update.contramap(_._2)) //pipe second element (OptimizerResult) to update sink
        .pipe(fst)
    }
  }


  object Experiment {
    case class SearchWord(space: ParamsSpace) {
      def minimize[T](what: Witness) = StrategyWord[what.T](StrategyMinimize, what, space)
    }

    case class StrategyWord[T](strategy: ExperimentStrategy, what: Witness, space: ParamsSpace)  {
      def in[R <: HList](expr: => LabelledTuple[R])(implicit selector: Selector[R, T]) =
        new Experiment(() => expr, space, LabelledTuple.selector[R, T], strategy).run()
    }

    def search(spaceDesc: ParamProvider) = SearchWord(spaceDesc.paramSpace)
  }
}

