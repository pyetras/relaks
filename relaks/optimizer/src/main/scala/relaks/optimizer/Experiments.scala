package relaks.optimizer

import relaks.data.LabelledTuples

import shapeless.ops.record.Selector
import shapeless.{Witness, HList}

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
    def run(): List[LabelledTuple[R]] = {
      val optimizer = Optimizer(space, strategy)

      @tailrec
      def loop(counter: Int, acc: List[LabelledTuple[R]]): List[LabelledTuple[R]] =
        if (counter <= 0) acc
        else optimizer.next() match {
          case Some(params) =>
            val newacc = currentValueStore.withValue(params) {
              val result = expr() // mozna pozbierac ktore zmienne sa tak faktycznie wywolywane
              optimizer.update(getObjective(result))
              result
            } :: acc
            loop(counter - 1, newacc)
          case None => acc
        }

      loop(100, List.empty)
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

