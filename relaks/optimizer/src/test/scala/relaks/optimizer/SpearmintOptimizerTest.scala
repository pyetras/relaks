package relaks.optimizer

/**
 * Created by Pietras on 05/05/15.
 */

import java.util.concurrent.{TimeoutException, ConcurrentLinkedQueue}

import org.scalatest._

import scalaz.{\/-, \/}
import scalaz.concurrent.Task
import scalaz.stream._
import scala.language.postfixOps
import scala.concurrent.duration._

class SpearmintOptimizerTest extends FunSpec with Matchers with Inside {

  class Spearmint extends SpearmintOptimizer {
    val q = async.unboundedQueue[String]

    val space = ParamProvider(
      "x" -> DiscreteRange(0, 50),
      "y" -> ChooseOneOf(List(-1, -2, -3, -4)))

    class SpearmintMock(parallel: Int, waitUpdate: Int) extends Spearmint(space.paramSpace, StrategyMinimize, parallel) {

      override protected lazy val initializeSpearmint: Task[Unit] = Task.now(())

      override protected val runSpearmint: Task[Int] = q.enqueueOne(s"run spearmint").map(x => 0)

      override protected def applyUpdate(optimizerResult: OptimizerResult): Task[Unit] =
        q.enqueueOne(s"apply update").flatMap(x => Task.delay { Thread.sleep(waitUpdate) })

      override protected def readNextPending(): Task[Unit \/ Params] = Task.now(\/-(Map.empty))

      def initSp = initializeSpearmint
    }

    def getSpMock(parallel: Int = 1, waitUpdate: Int = 0) = new SpearmintMock(parallel, waitUpdate)
    def getSp(parallel: Int = 1) = new Spearmint(space.paramSpace, StrategyMinimize, parallel)
    def getSpaceJson = space.toSpearmintJson
    def dumpQueue = q.dequeueAvailable.take(1).runLast.run.get
  }

  val runSpearmint = "^(run spearmint).*".r
  val applyUpdate = "^(apply update).*".r

  val to = new TimeoutException()
  type TimeoutExceptionT = to.type

  describe("Spearmint Nondet Param Extension") {
    it("should serialize a parameter space to Json") {
      import org.json4s.jackson.JsonMethods._
      val Spearmint = new Spearmint

      println(compact(render(Spearmint.getSpaceJson)))
    }
  }

  describe("Spearmint optimizer") {
    it("should generate params when no updates have been applied") {
      val Spearmint = new Spearmint
      val sp = Spearmint.getSpMock()

      sp.paramStream.take(1).run.runFor(1000 milliseconds)
      Spearmint.dumpQueue should contain only "run spearmint"

    }

    it("should update before generating when updates are pending") {
      val Spearmint = new Spearmint
      val sp = Spearmint.getSpMock()

      sp.paramStream.pipe(process1.lift { params =>
        Spearmint.OptimizerResult(0, params)
      }).to(sp.update).take(2).run.runFor(1000 milliseconds)

      Spearmint.dumpQueue should contain theSameElementsAs Seq("run spearmint", "apply update", "run spearmint")
    }

    it("should allow only maxParallel evals and then block until update") {
      import scala.concurrent.duration._

      val Spearmint = new Spearmint
      var sp = Spearmint.getSpMock(2)

      noException shouldBe thrownBy (sp.paramStream.take(2).run.runFor(10 milliseconds))

      sp = Spearmint.getSpMock(2)

      a [TimeoutExceptionT] shouldBe thrownBy (sp.paramStream.take(3).run.runFor(100 milliseconds))

      sp = Spearmint.getSpMock(2)

      noException shouldBe thrownBy {
        sp.paramStream.pipe(process1.lift { params =>
          Spearmint.OptimizerResult(0, params)
        }).to(sp.update).take(3).run.runFor(100 milliseconds)
      }
    }

    it("should allow runnning in parallel") {
      def parallelTask() = {
        val Spearmint = new Spearmint
        val sp = Spearmint.getSpMock(2)

        val chan: Channel[Task, Spearmint.Params, Process[Task, Spearmint.OptimizerResult]] = channel.lift {
          (params: Spearmint.Params) =>
            Task {
              Process.eval {
                Task.delay {
                  Thread.sleep(500)
                  Spearmint.OptimizerResult(0, params)
                }
              }
            }
        }
        (nondeterminism.njoin(2, 1)(sp.paramStream.through(chan)).to(sp.update).take(4).run, Spearmint.q)
      }

      val (task1, q1) = parallelTask()
      //sanity check - it should take at least 500ms
      task1.runAsync(x => ())
      a [TimeoutExceptionT] shouldBe thrownBy { q1.dequeue.take(1 + 1 + 2).run.runFor(400 milliseconds) }

      val (task2, q2) = parallelTask()
      task2.runAsync(x => ())
      //sequenced takes >= 200 ms
      noException shouldBe thrownBy { q2.dequeue.take(1 + 1 + 2 + 2).run.runFor(999 milliseconds) }
    }
    it("should dequeue updates first (when available)") {
      pending
    }
  }
}
