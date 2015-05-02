package relaks.optimizer

import scalaz._
import Scalaz._
import scalaz.concurrent._
import scalaz.stream._
import scalaz.stream.async.mutable.Queue

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetVars {
  sealed class ExperimentStrategy
  object StrategyMinimize extends ExperimentStrategy

  case class OptimizerResult[+T](result: T)

  trait Optimizer {
    def paramStream: Process[Task, ValStore]
    def update: Sink[Task, OptimizerResult[Any]]
  }

  object Optimizer {
    def apply(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy) = initOptimizer(spaceDesc, strategy)
  }

  protected def initOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy): Optimizer
}

trait GridOptimizer extends BaseOptimizer {
  class GrOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy) extends Optimizer {
    var generator = spaceDesc.foldLeft(Seq(Seq.empty[(String, Any)])) ((acc, keyval) =>
      acc.view.flatMap((seq) => keyval._2.view.map((el) => seq :+ (keyval._1, el))))


    override def paramStream: Process[Task, ValStore] = Process.unfold(generator) { (lst) =>
      lst.headOption.map(params => (params.toMap, lst.tail))
    }

    override def update: Sink[Task, OptimizerResult[Any]] = sink.lift(x => Task.now(()))
  }
  override protected def initOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy): Optimizer = new GrOptimizer(spaceDesc, strategy)
}