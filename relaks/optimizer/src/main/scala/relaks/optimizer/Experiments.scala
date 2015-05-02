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

trait Experiments extends NondetVars with BaseOptimizer with LabelledTuples {

  implicit val currentValueStore = new DynamicVariable(Map[String, Any]())
  implicit def extract[T: TypeTag](from: Nondet): T = currentValueStore.value(from.name).asInstanceOf[T]

  implicit class NondetOps(self: Nondet) {
    def as[T: TypeTag]: T = extract(self)
  }

  class Experiment[R <: HList, O](expr: () => LabelledTuple[R], space: VarSpaceDesc, getObjective: LabelledTupleSelector[R, O], strategy: ExperimentStrategy) {
    def run(): Process[Task, LabelledTuple[R]] = {
      val optimizer = Optimizer(space, strategy)

      val loop: Process1[ValStore, (LabelledTuple[R], OptimizerResult[O])] = process1.lift { params =>
        val resultTuple = currentValueStore.withValue(params) {
          expr() // mozna pozbierac ktore zmienne sa tak faktycznie wywolywane...
        }
        (resultTuple, OptimizerResult(getObjective(resultTuple)))
      }

      def fst[L]: Process1[(L, _), L] = process1.lift {_._1}
//      def snd[R]: Process1[(_, R), R] = process1.lift {_._2}

      optimizer.paramStream
        .pipe(loop)
        .observe(optimizer.update.contramap(_._2)) //pipe second element (OptimizerResult) to update sink
        .pipe(fst)
    }
  }


  object Experiment {
    case class SearchWord(space: VarSpaceDesc) {
      def minimize[T](what: Witness) = StrategyWord[what.T](StrategyMinimize, what, space)
    }

    case class StrategyWord[T](strategy: ExperimentStrategy, what: Witness, space: VarSpaceDesc)  {
      def in[R <: HList](expr: => LabelledTuple[R])(implicit selector: Selector[R, T]) =
        new Experiment(() => expr, space, LabelledTuple.selector[R, T], strategy).run()
    }

    def search(spaceDesc: VarProvider) = SearchWord(spaceDesc.vars)
  }
}

