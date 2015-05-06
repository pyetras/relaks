package relaks.optimizer.util

import scala.sys.process._
import scalaz.concurrent._
import scalaz.stream._
import scalaz.stream.Process

/**
 * Created by Pietras on 04/05/15.
 */
object StreamProcess {
  sealed trait SPResult
  case class ExitCode(code: Int) extends SPResult
  sealed trait SPO extends SPResult
  case class OutLine(line: String) extends SPO
  case class ErrLine(line: String) extends SPO


  def apply(cmd: Seq[String]) = {
    val stdout, stderr = async.unboundedQueue[String] //TODO
    //run async
    val proc = Task {
        cmd run new ProcessIO(
          sin => sin.close(),
          sout => io.linesR(sout).to(stdout.enqueue).onComplete(Process eval stdout.close).run.run,
          serr => io.linesR(serr).to(stderr.enqueue).onComplete(Process eval stderr.close).run.run)
      }

    val spProcess: Process[Task, Int] = for {
      p <- Process eval proc
      throwable <- Process.eval { if(p.exitValue() == 0) Task.now(0) else Task.fail(new IllegalArgumentException) } // TODO exception
    } yield throwable

    val merged: Process[Task, SPResult] = spProcess.map(ExitCode).wye(
      stdout.dequeue.map(OutLine).wye(stderr.dequeue.map(ErrLine))(wye.merge)
    )(wye.merge)

    merged
  }

}

class StrProcOps(val self: Process[Task, StreamProcess.SPResult]) extends AnyVal {
  def stdin = ???
  def stderr = ???
  def errCode = ???
}

