package relaks.optimizer

import relaks.optimizer.util.StreamProcess

import scala.sys.process._
import scala.util.Try
import scalaz._
import scalaz.concurrent._
import scalaz.stream.Process
import scalaz.stream._

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetParams {
  sealed case class OptimizerResult(result: Any, params: Params, time: Int = 0)
  object EmptyResult extends OptimizerResult(0, Map.empty, 0)

  sealed class ExperimentStrategy
  object StrategyMinimize extends ExperimentStrategy

//  case class OptimizerResult[+T](result: T)

  trait Optimizer {
    def paramStream: Process[Task, Params]
    def update: Sink[Task, OptimizerResult]
  }

  object Optimizer {
    def apply(spaceDesc: ParamsSpace, strategy: ExperimentStrategy, maxParallel: Int = 1) = initOptimizer(maxParallel, spaceDesc, strategy)
  }

  protected def initOptimizer(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer
}

trait GridOptimizer extends BaseOptimizer {

  class GrOptimizer(spaceDesc: ParamsSpace, strategy: ExperimentStrategy) extends Optimizer {
    // a lazy generator for cartesian product of variable space
    var generator = spaceDesc.foldLeft(Seq(Seq.empty[(String, Any)])) ((acc, keyval) =>
      acc.view.flatMap((seq) => keyval._2.view.map((el) => seq :+ (keyval._1, el))))


    override def paramStream: Process[Task, Params] = Process.unfold(generator) { (lst) =>
      lst.headOption.map(params => (params.toMap, lst.tail))
    }

    override def update: Sink[Task, OptimizerResult] = sink.lift(x => Task.now(()))
  }
  override protected def initOptimizer(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer =
    new GrOptimizer(spaceDesc, strategy)
}

trait SpearmintOptimizer extends BaseOptimizer {
  class Spearmint(spaceDesc: ParamsSpace, strategy: ExperimentStrategy, maxParallel: Int) extends Optimizer {
    private var spearmintResult = Map.empty[Params, OptimizerResult]
    private var spearmintPending = Set.empty[Params]

    protected def runSpearmint = {
      val script = getClass.getResource("/runSpearmint.sh").getPath
      StreamProcess(Seq("/bin/sh", "-c", s"sh $script braninpy"))
      //dummy
      Task.delay(println("Run spearmint"))
    }

    val initial: Process[Task, Unit] = Process.constant(()).take(maxParallel)

    //grabs updates and applies them synchronously
//    private lazy val updateDaemon: Sink[Task, OptimizerResult] = {
//      val q = async.unboundedQueue[OptimizerResult]
//
//      q.dequeue.to(sink.lift(applyUpdate)).run.runAsync {
//        case -\/(t: Throwable) => println(t)
//      }
//
//      q.enqueue
//    }

    val ticketQueue = async.unboundedQueue[Unit]

    //pending updates
    val updateQueue = async.unboundedQueue[OptimizerResult]
    
    protected val paramGenerator: Process[Task, Params] = {
      //initialize ticket queue
      val init: Process[Task, Unit] = Process.constant(()).take(maxParallel)
      val ticketInit = init.to(ticketQueue.enqueue)

      //grab from updated or initial guesses or already applied updates TODO: should grab from updated first
      //dequeue available blocks (never returns empty)
      val updatesOrFresh = wye(updateQueue.dequeueAvailable, ticketQueue.dequeue)(wye.either)

      //if there are any updates available apply them first
      //tries to update as many as possible before making another guess
      def uOFMapper(res: Seq[OptimizerResult] \/ Unit): Process[Task, Nothing] = res match {
        case -\/(resultlst) => Process.eval_ {
          //apply all updates
          val update = resultlst.foldLeft(Task.now(()))((task, result) => task.flatMap(x => applyUpdate(result)))

          //put back updates.length - 1 tickets to ticketQueue TODO: it should ensure the tickets are returned in case of error
          update.flatMap { x =>
            val generator: Process[Task, Unit] = Process.constant(()).take(resultlst.length - 1)
            generator.to(ticketQueue.enqueue).run
          }
        }
        case \/-(()) => Process.halt
      }

      for {
        //insert all initially available tickets
        _ <- ticketInit.last
        //apply updates...
        res <- updatesOrFresh
        //and then run spearmint
        results <- uOFMapper(res) ++ Process.eval(runSpearmint.map(x => readNextPending()))
      } yield results
    }

    protected def applyUpdate(optimizerResult: OptimizerResult): Task[Unit] = {
      Task.delay(Task.Try {
        println(s"appending update $optimizerResult")
        if (spearmintResult.get(optimizerResult.params).nonEmpty) {
          throw new IllegalArgumentException(s"Params ${optimizerResult.params} evaluated more than once") // TODO exception type
        } else if (!spearmintPending.contains(optimizerResult.params)) {
          throw new IllegalArgumentException(s"Params ${optimizerResult.params} are not known to be pending evaluation")
        } else {
          spearmintPending = spearmintPending - optimizerResult.params
          spearmintResult = spearmintResult + (optimizerResult.params -> optimizerResult)
        }
      })
    }

    //side effect: appends to spearmintPending
    protected def readNextPending(): Params = ???

    override def paramStream: Process[Task, Params] = paramGenerator

    override val update: Sink[Task, OptimizerResult] = updateQueue.enqueue
  }

  override protected def initOptimizer(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer =
    new Spearmint(spaceDesc, strategy, maxParallel)
}