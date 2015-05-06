package relaks.optimizer

/**
 * Created by Pietras on 05/05/15.
 */

import java.util.concurrent.ConcurrentLinkedQueue

import org.scalatest._

import scalaz.concurrent.Task
import scalaz.stream._
import scala.language.postfixOps
import scala.concurrent.duration._

class SpearmintOptimizerTest extends FunSpec with Matchers with Inside {

  class Spearmint extends SpearmintOptimizer {
    val q = async.unboundedQueue[String]

    val space = ParamProvider(Map(
      "x" -> DiscreteRange(0, 50),
      "y" -> ChooseOneOf(List(-1, -2, -3, -4))))

    class SpearmintMock(parallel: Int, waitUpdate: Int) extends Spearmint(space.paramSpace, StrategyMinimize, parallel) {
      override protected def runSpearmint: Task[Unit] = q.enqueueOne(s"run spearmint")

      override protected def applyUpdate(optimizerResult: OptimizerResult): Task[Unit] = q.enqueueOne(s"apply update").flatMap(x => Task.delay { Thread.sleep(waitUpdate) })

      //side effect: appends to spearmintPending
      override protected def readNextPending(): Params = Map.empty
    }

    def getSp(parallel: Int = 1, waitUpdate: Int = 0) = new SpearmintMock(parallel, waitUpdate)
    def dumpQueue = q.dequeueAvailable.take(1).runLast.run.get
  }

  val runSpearmint = "^(run spearmint).*".r
  val applyUpdate = "^(apply update).*".r

  describe("Spearmint optimizer") {
    it("should generate params when no updates have been applied") {
      val Spearmint = new Spearmint
      val sp = Spearmint.getSp()
      sp.paramStream.take(1).run.run
      Spearmint.dumpQueue should contain only "run spearmint"
    }

    it("should update before generating when updates are pending") {
      val Spearmint = new Spearmint
      val sp = Spearmint.getSp()

      sp.paramStream.pipe(process1.lift { params =>
        Spearmint.OptimizerResult(0, params)
      }).to(sp.update).take(2).run.run

      Spearmint.dumpQueue should contain theSameElementsAs Seq("run spearmint", "apply update", "run spearmint")
    }

    it("should allow only maxParallel evals and then block until update") {
      import scala.concurrent.duration._
      import java.util.concurrent.TimeoutException

      val Spearmint = new Spearmint
      var sp = Spearmint.getSp(2)

      noException shouldBe thrownBy (sp.paramStream.take(2).run.runFor(10 milliseconds))

      sp = Spearmint.getSp(2)

      val to = new TimeoutException()
      a [to.type] shouldBe thrownBy (sp.paramStream.take(3).run.runFor(100 milliseconds))

      sp = Spearmint.getSp(2)

      noException shouldBe thrownBy {
        sp.paramStream.pipe(process1.lift { params =>
          Spearmint.OptimizerResult(0, params)
        }).to(sp.update).take(3).run.run
      }
    }

    it("should allow runnning in parallel") {

      val Spearmint = new Spearmint
      val sp = Spearmint.getSp(2)

      val chan: Channel[Task, Spearmint.Params, Process[Task, Spearmint.OptimizerResult]] = channel.lift {
        (params: Spearmint.Params) =>
          Task {
            Process.eval {
              Task.delay {
                Thread.sleep(100)
                Spearmint.OptimizerResult(0, params)
              }
            }
          }
      }
      nondeterminism.njoin(2, 1)(sp.paramStream.through(chan)).to(sp.update).take(4).run.runAsync(x => ())
      //sequenced takes >= 200 ms
      noException shouldBe thrownBy { Spearmint.q.dequeue.take(1 + 1 + 2 + 2).run.runFor(199 milliseconds) }
    }

  }
}
