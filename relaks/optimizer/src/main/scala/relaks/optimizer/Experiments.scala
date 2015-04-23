package relaks.optimizer

import shapeless.ops.record.Selector
import shapeless.{Witness, HList}

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag
import scala.util.DynamicVariable
import scala.language.implicitConversions

/**
 * Created by Pietras on 23/04/15.
 */

trait Experiments extends NondetVars with BaseOptimizer {

  implicit val currentValueStore = new DynamicVariable(Map[String, Any]())
  implicit def extract[T: TypeTag](from: Nondet): T = currentValueStore.value(from.name).asInstanceOf[T]

  class Experiment[R <: HList, O](expr: () => ExperimentResult[R], space: VarSpaceDesc, getObjective: Sel[R, O], strategy: ExperimentStrategy) {
    def run(): List[R] = {
      initializeOptimizer()

      @tailrec
      def loop(counter: Int, acc: List[R]): List[R] =
        if (counter <= 0) acc
        else getNextParams match {
          case Some(params) =>
            val newacc = currentValueStore.withValue(params) {
              val result = expr() // mozna pozbierac ktore zmienne sa tak faktycznie wywolywane
              updateObjectiveValue(getObjective(result()))
              result()
            } :: acc
            loop(counter - 1, newacc)
          case None => acc
        }

      loop(100, List.empty)
    }

    private def initializeOptimizer(): Unit = Optimizer.initialize(space, strategy)

    private def getNextParams: Option[ValStore] = Optimizer.nextParams()

    private def updateObjectiveValue(obj: O): Unit = Optimizer.updateObjectiveValue(obj)
  }

  trait Sel[R <: HList, O]{
    def apply(r: R): O
  }

  object Experiment {
    case class SearchWord(space: VarSpaceDesc) {
      def minimize[T](what: Witness) = StrategyWord[what.T](StrategyMinimize, what, space)
    }

    case class StrategyWord[T](strategy: ExperimentStrategy, what: Witness, space: VarSpaceDesc)  {
      def in[R <: HList](expr: => ExperimentResult[R])(implicit selector: Selector[R, T]) =
        new Experiment(() => expr, space, new Sel[R, selector.Out] { def apply(r: R) = selector(r) }, strategy).run()
    }

    def search(spaceDesc: VarProvider) = SearchWord(spaceDesc.vars)
  }

  case class ExperimentResult[R <: HList](result: R) {
    def apply(): R = result
  }
}

