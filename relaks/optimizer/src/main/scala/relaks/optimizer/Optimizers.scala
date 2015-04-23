package relaks.optimizer

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetVars {
  sealed class ExperimentStrategy
  object StrategyMinimize extends ExperimentStrategy

  object Optimizer {
    def initialize(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy) = initOptimizer(spaceDesc, strategy)
    def nextParams() = next()
    def updateObjectiveValue(o: Any) = update(o)
  }

  protected def initOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy): Unit
  protected def next(): Option[ValStore]
  protected def update(o: Any): Unit
}

trait GridOptimizer extends BaseOptimizer {
  var generator: Seq[Seq[(String, Any)]] = _

  override protected def initOptimizer(spaceDesc: VarSpaceDesc, strategy: ExperimentStrategy): Unit = {
    generator = spaceDesc.foldLeft(Seq(Seq.empty[(String, Any)])) ((acc, keyval) =>
      acc.view.flatMap((seq) => keyval._2.view.map((el) => seq :+ (keyval._1, el))))
  }
  override protected def next(): Option[ValStore] =
    generator.headOption.map((head) => {
      generator = generator.tail
      head.toMap
    })

  override protected def update(o: Any): Unit = ()
}