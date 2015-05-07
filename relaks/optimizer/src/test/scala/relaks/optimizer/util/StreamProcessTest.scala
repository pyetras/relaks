package relaks.optimizer.util

/**
 * Created by Pietras on 06/05/15.
 */

import java.io.{ByteArrayOutputStream, PrintStream}

import syntax._
import scalaz.concurrent.Task
import scalaz.stream._
import org.scalatest._
import scala.language.postfixOps
import scala.concurrent.duration._

class StreamProcessTest extends FunSpec with Matchers with Inside {
  def withByteStreams(f: (Sink[Task, String], Sink[Task, String]) => Task[Int]) = {
    val osout, oserr = new ByteArrayOutputStream()
    (osout, oserr, f(io.printLines(new PrintStream(osout)), io.printLines(new PrintStream(oserr))))
  }

  describe("StreamProcess") {
    it("should create a task that writes to output streams and returns exit code") {
      val (osout, oserr, task) = withByteStreams(StreamProcess("uname").withOutput)

      task.runFor(1000 milliseconds) should equal(0)

      osout.toString.trim.split("\n").length should be > 0
      oserr.toString.trim should equal("")
    }

    it("should run a command in a shell") {
      val (osout, oserr, task) = withByteStreams(StreamProcess.shell("ls", "-la").withOutput)

      task.runFor(1000 milliseconds) should equal(0)

      //. and ..
      osout.toString.trim.split("\n").length should be >= 2
      oserr.toString.trim should equal("")
    }
  }
}
