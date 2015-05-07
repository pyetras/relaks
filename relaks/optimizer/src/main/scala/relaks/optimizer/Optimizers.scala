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

/**
 * Created by Pietras on 23/04/15.
 */
trait BaseOptimizer extends NondetParams {
  sealed case class OptimizerResult(result: Any, params: Params, time: Long = 0)
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

trait SpearmintOptimizer extends BaseOptimizer with NondetParamExtensions.Spearmint with LazyLogging {
  import org.json4s.jackson.JsonMethods._
  import util.syntax._

  class Spearmint(spaceDesc: ParamsSpace, strategy: ExperimentStrategy, maxParallel: Int) extends Optimizer {
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
    }//.flatMap(x => io.linesR(resultsPath.toString).to(sink.lift(x => Task.now(logger.debug(x)))).run)
    /**
     * @return dumps results.dat and runs spearmint
     */
    protected def runSpearmint: Task[Int] = { //TODO make this a val by making streamprocess command lazy
      val script = this.getClass.getResource("/runSpearmint.sh").getPath
      val proc = StreamProcess.shell(s"sh $script --method=GPEIChooser $directoryPath")
        .withOutput(sink.lift(line => Task.now(logger.debug(line))), sink.lift(line => Task.now(logger.debug(line))))
      updateResults.flatMap(x => proc)
    }

    protected lazy val initializeSpearmint: Task[Unit] = {
      directoryPath = Files.createTempDirectory("spearmint")
      logger.debug(s"spearmint dir: $directoryPath")
      val fileLineWriter: Process[Task, String => Task[Unit]] = io.asyncFileLinesW(configPath)

      Process.eval(Task.delay { compact(render(spaceDesc.toSpearmintJson)) }).through(fileLineWriter).run
    }

    private val ticketQueue = async.unboundedQueue[Unit]

    //pending updates
    private val updateQueue = async.unboundedQueue[OptimizerResult]
    
    protected val paramGenerator: Process[Task, Params] = {
      //initialize ticket queue
      val ticketInit: Process[Task, Unit] = Process.constant(()).take(maxParallel)
      val init: Process[Task, Unit] = Process.eval_(initializeSpearmint) ++ ticketInit.to(ticketQueue.enqueue)

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

      def handleErrCode(code: Int): Task[Params] = {
        def go(code: Int, accumulator: Int): Task[Params] = {
          if (accumulator > 10)
            Task.fail(new RuntimeException("spearmint converged"))
          else
            code match {
              case 0 => readNextPending().flatMap {
                case -\/(()) => runSpearmint flatMap (code => Task.suspend(go(code, accumulator + 1)))
                case \/-(params) => Task.now(params)
              }
              case code@_ => Task.fail(new RuntimeException(s"spearmint finished with nonzero exit code $code")) //TODO Exception type
            }
        }
        go(code, 0)
      }

      for {
        //insert all initially available tickets
        _ <- init.last
        //apply updates...
        res <- updatesOrFresh
        //and then run spearmint
        results <- uOFMapper(res) ++ Process.eval(runSpearmint flatMap handleErrCode)
      } yield results
    }

    protected def applyUpdate(optimizerResult: OptimizerResult): Task[Unit] = {
      Task.suspend{
        logger.debug(s"spearmint: appending update $optimizerResult")
        if (!spearmintPending.contains(optimizerResult.params)) {
          Task fail new IllegalArgumentException(s"Params ${optimizerResult.params} are not known to be pending evaluation")
        } else {
          spearmintPending = spearmintPending - optimizerResult.params

          val prevSize = spearmintResultMemo.size
          spearmintResultMemo += (optimizerResult.params -> optimizerResult)
          spearmintEvals += optimizerResult.params
          if (prevSize == spearmintResultMemo.size)
            Task fail new IllegalArgumentException(s"Params ${optimizerResult.params} evaluated more than once") // redundant, checked in readNextPending TODO exception type
          else
            Task.now(())
        }
      }
    }

    /**
     * reads from results.dat
     * side effect: appends to spearmintPending
     *
     * @return params newly added to results.dat
     */
    protected def readNextPending(): Task[Unit \/ Params] = {
      //read results.dat, filter pending files
      val pendings = io.linesR(resultsPath.toString)/*.observe(sink.lift(x => Task.now(logger.debug(x))))*/.map(_.split(" ")).filter(_(0) == "P")
        .map(spaceDesc.paramsFromSpearmintResults)
        .flatMap(x => Process.eval(tryToTask(x))).runLog

      //find one that is not in the pendingSet
      pendings.flatMap { paramsl =>
        paramsl.toSet.diff(spearmintPending).headOption match {
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
          case None => Task.fail(new RuntimeException("No new pending params"))
        }
      }
    }

    override def paramStream: Process[Task, Params] = paramGenerator

    override val update: Sink[Task, OptimizerResult] = updateQueue.enqueue
  }

  override protected def initOptimizer(maxParallel: Int, spaceDesc: ParamsSpace, strategy: ExperimentStrategy): Optimizer =
    new Spearmint(spaceDesc, strategy, maxParallel)
}