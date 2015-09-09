package relaks.optimizer

import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.{CompletionHandler, AsynchronousFileChannel}
import java.nio.file.{StandardOpenOption, Paths, Files, Path}

import com.typesafe.scalalogging.LazyLogging
import relaks.optimizer.util.StreamProcess

import scala.collection.mutable
import scala.sys.process._
import scala.util.Try
import scalaz._
import scalaz.concurrent._
import scalaz.stream.Process
import scalaz.stream._
import Scalaz._

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetParams {

//  case class OptimizerResult[+T](result: T)

  trait Optimizer {
    def paramStream: Process[Task, Params]
    def update: Sink[Task, OptimizerResult]
  }
  sealed case class OptimizerResult(result: Any, params: Params, time: Long = 0)
  object EmptyResult extends OptimizerResult(0, Map.empty, 0)

  sealed class ExperimentStrategy
  object StrategyMinimize extends ExperimentStrategy

  def apply(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer
}

trait FatalOptimizerError extends Exception
trait OptimizerError extends Exception
class OptimizerConvergedError extends RuntimeException("Optimizer converged") with OptimizerError

trait GridOptimizer extends BaseOptimizer {
  class GrOptimizer(spaceDesc: ParamsSpace, strategy: ExperimentStrategy) extends Optimizer {
    override def paramStream: Process[Task, Params] = {
      // a lazy generator for cartesian product of variable space
      def go(acc: Params, rest: Seq[(String, NondetParam[Any])]): Process[Task, Params] =
        rest match {
          case Seq() => Process.emit(acc)
          case (name, vals) +: tail => vals.view.foldLeft(Process.empty[Task, Map[String, Any]]) {
            (proc, value) =>
              proc ++ go(acc + (name -> value), tail)
          }
        }
      Process.suspend(go(Map.empty, spaceDesc.toStream))
    }

    override def update: Sink[Task, OptimizerResult] = sink.lift(x => Task.now(()))
  }
  override def apply(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer =
    new GrOptimizer(spaceDesc, strategy)
}

object GridOptimizer extends GridOptimizer

//todo make optimizer a monad
trait SpearmintOptimizer extends BaseOptimizer with NondetParamExtensions.Spearmint with LazyLogging {
  import org.json4s.jackson.JsonMethods._
  import util.syntax._

  class Spearmint(spaceDesc: ParamsSpace,
                  strategy: ExperimentStrategy,
                  maxParallel: Int,
                  convergeAfter: Int = 10,
                  scriptArgs: String = "--method=GPEIChooser") extends Optimizer {
    private val spearmintResultMemo = mutable.LinkedHashMap.empty[Params, OptimizerResult]
    private val spearmintEvals = mutable.MutableList.empty[Params]
    private var spearmintPending = Set.empty[Params]
    private var directoryPath: Path = _
    private lazy val resultsPath: Path = directoryPath.resolve("results.dat")
    private lazy val configPath: Path = directoryPath.resolve("config.json")

    private val updateResults = Task.delay {
      def resultsToResultsString(result: OptimizerResult): String = s"${result.result} ${result.time} ${paramsToResultsString(result.params)}"
      def paramsToResultsString(params: Params): String = spaceDesc.map(kv => kv._2.toStringResult(params.apply(kv._1))).mkString(" ")
      def pendingToResultsString(params: Params): String = s"P P ${paramsToResultsString(params)}"
      val printer = new PrintWriter(resultsPath.toFile)
      (spearmintEvals.map(spearmintResultMemo).map(resultsToResultsString) ++ spearmintPending.map(pendingToResultsString)).foreach(printer.println)
      printer.close()
    }

    /**
     * dumps results.dat and runs spearmint
     * @return spearmint process exit code
     */
    protected def runSpearmint: Task[Int] = { //TODO make this a val by making streamprocess command lazy
      val script = this.getClass.getResource("/runSpearmint.sh").getPath
      val proc = StreamProcess.shell(s"sh $script $scriptArgs $directoryPath")
        .withOutput(sink.lift(line => Task.now(logger.debug(line))), sink.lift(line => Task.now(logger.debug(line))))
      updateResults.flatMap(x => proc)
    }

    protected lazy val initializeSpearmint: Task[Unit] = {
      directoryPath = Files.createTempDirectory("spearmint")
      logger.debug(s"spearmint dir: $directoryPath")
      val fileLineWriter: Process[Task, String => Task[Unit]] = io.asyncFileLinesW(configPath)

      Process.eval(Task.delay { compact(render(spaceDesc.toSpearmintJson)) }).through(fileLineWriter).run
    }

    //pending updates
    private val updateTicketQueue = async.unboundedQueue[OptimizerResult \/ Unit]
    
    protected lazy val paramGenerator: Process[Task, Params] = {

      //initialize ticket queue
      val ticketInit: Task[Unit] = updateTicketQueue.enqueueAll(for (_ <- 1 to maxParallel) yield ().right)
      val init: Task[Unit] = initializeSpearmint.flatMap(_ => ticketInit)

      //if there are any updates available apply them first
      //tries to update as many as possible before making another guess
      val updateWithTicket: Process[Task, Seq[OptimizerResult \/ Unit]] =
        updateTicketQueue.dequeueAvailable//.pipe(chunkUntil(seq => seq.exists(_.isRight))).map(_.flatten)

      val applyUpdates: Process[Task, Unit] = updateWithTicket.flatMap { seq =>
        val updates = seq.collect {
          case -\/(u) => u
        }
        val update = updates.foldLeft(Task.now(()))((task, result) => task.flatMap(x => applyUpdate(result)))

        val tickets = seq.filter(_.isRight)

        val safeTicket = Process.eval(Task.now(())).take(1)
          .onFailure(throwable =>
            Process
              .eval_(updateTicketQueue.enqueueOne(().right)
                .flatMap(_ => Task.fail(throwable)))
          )

        val leftTickets = (updates.indices.map(_ => ().right[OptimizerResult]) ++ tickets).tail

        Process.eval(update.flatMap(_ => updateTicketQueue.enqueueAll(leftTickets))).take(1).drain ++
          safeTicket
      }


      /**
       * to be called after spearmint update. If a call to readNextPending
       * returns a unit loops runSpearmint until a new params are generated
       */
      def getNextParams: Task[Params] = { //TODO why this doesn't work as val???
        def go(code: Int, accumulator: Int): Task[Params] = {
          if (accumulator > convergeAfter)
            Task.fail(new OptimizerConvergedError)
          else
            code match {
              case 0 => readPending.take(1).runLast.map(_.get).flatMap {
                case -\/(()) => runSpearmint flatMap (code => Task.suspend(go(code, accumulator + 1)))
                case \/-(params) => Task.now(params)
              }
              case code@_ => Task.fail(new RuntimeException(s"spearmint finished with nonzero exit code $code") with FatalOptimizerError)
            }
        }
        runSpearmint flatMap (code => go(code, 0))
      }

      Process.eval_(init) ++ applyUpdates.evalMap(_ => getNextParams)
    }

    /**
     * side effect: stores optimizerResult in spearmintResultMemo and in
     * spearmintEvals.
     *
     * @param optimizerResult
     * @return
     */
    protected def applyUpdate(optimizerResult: OptimizerResult): Task[Unit] = {
      Task.suspend {
        logger.debug(s"spearmint: appending update $optimizerResult")
        if (!spearmintPending.contains(optimizerResult.params)) {
          Task fail new IllegalArgumentException(s"Params ${optimizerResult.params} are not known to be pending evaluation") with FatalOptimizerError
        } else {
          spearmintPending = spearmintPending - optimizerResult.params

          val prevSize = spearmintResultMemo.size
          spearmintResultMemo += (optimizerResult.params -> optimizerResult)
          spearmintEvals += optimizerResult.params
          if (prevSize == spearmintResultMemo.size)
            Task fail new IllegalArgumentException(s"Params ${optimizerResult.params} evaluated more than once") with FatalOptimizerError
          else
            Task.now(())
        }
      }
    }

    /**
     * reads from results.dat
     * side effect: appends to spearmintPending
     *
     * @return params newly added to results.dat or unit if spearmint
     *         requested params that have already been evaluated.
     *         why would it do that? no idea, happens pretty often with
     *         GPEIChooser and branin
     */
    protected lazy val readPending: Process[Task, Unit \/ Params] = {
      //read results.dat, filter pending files
      val pendings = io.linesR(resultsPath.toString).map(_.split(" ")).filter(_(0) == "P")
        .map(spaceDesc.paramsFromSpearmintResults)
        .flatMap(x => Process.eval(tryToTask(x)))

      //find one that is not in the pendingSet
      pendings.filter(p => !spearmintPending.contains(p)).awaitOption
        .evalMap({
        case Some(param) =>
          logger.debug(s"got new params $param")
          if (spearmintResultMemo.contains(param)) {
            logger.warn(s"spearmint tried to evaluate $param again")
            spearmintEvals += param
            Task.now(-\/(()))
          } else {
            spearmintPending = spearmintPending + param
            Task.now(\/-(param))
          }
        case None => Task.fail(new RuntimeException("No new pending params") with FatalOptimizerError)
      })
    }

    override def paramStream: Process[Task, Params] = paramGenerator

    override val update: Sink[Task, OptimizerResult] = updateTicketQueue.enqueue.contramap(x => x.left)
  }

  override def apply(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer =
    new Spearmint(spaceDesc, strategy, maxParallel)

  def apply(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy,
                     convergeAfter: Int, spearmintArgs: String): Optimizer =
    new Spearmint(spaceDesc, strategy, maxParallel, convergeAfter, spearmintArgs)
}

object SpearmintOptimizer extends SpearmintOptimizer