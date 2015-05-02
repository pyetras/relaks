package relaks.optimizer

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetVars {
  sealed class ExperimentStrategy
  object StrategyMinimize extends ExperimentStrategy

  trait Optimizer {
    def next(): Option[ValStore]
    def update(o: Any): Unit
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

    override def next(): Option[ValStore] =
      generator.headOption.map((head) => {
        generator = generator.tail
        head.toMap
      })

    override def update(o: Any): Unit = ()
  }
  override protected def initOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy): Optimizer = new GrOptimizer(spaceDesc, strategy)
}