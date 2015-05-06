package relaks.optimizer.util

import java.io.OutputStream

import scala.sys.process._
import scalaz._
import Scalaz._
import scalaz.concurrent._
import scalaz.stream._
import scalaz.stream.Process
import scala.language.implicitConversions

/**
 * Created by Pietras on 04/05/15.
 */
object StreamProcess {
  sealed trait SPResult
  case class ExitCode(code: Int) extends SPResult
  sealed trait SPO extends SPResult
  case class OutLine(line: String) extends SPO
  case class ErrLine(line: String) extends SPO


  def apply(cmd: String*): Process[Task, SPResult] = {
    val stdout, stderr = async.unboundedQueue[String] //TODO lets do something more efficient than queue
    //run async
    val proc = Task {
        cmd run new ProcessIO(
          sin => sin.close(),
          sout => io.linesR(sout).to(stdout.enqueue).onComplete(Process eval stdout.close).run.run,
          serr => io.linesR(serr).to(stderr.enqueue).onComplete(Process eval stderr.close).run.run)
      }

    val spProcess: Process[Task, Int] = for {
      p <- Process eval proc
//      throwable <- Process.eval { if(p.exitValue() == 0) Task.now(0) else Task.fail(new IllegalArgumentException) } // TODO exception
    } yield p.exitValue()

    val merged: Process[Task, SPResult] = spProcess.map(ExitCode).wye(
      stdout.dequeue.map(OutLine).wye(stderr.dequeue.map(ErrLine))(wye.merge)
    )(wye.merge)

    merged
  }

  object syntax {
    implicit def addStrProcOps(proc: Process[Task, SPResult]): StrProcOps = new StrProcOps(proc)
  }

}

class StrProcOps(val self: Process[Task, StreamProcess.SPResult]) extends AnyVal {
  def withOutput(sout: Sink[Task, String], serr: Sink[Task, String]): Task[Int] = {
    val justCode: Process[Task, Unit \/ Int] = self.flatMap {
      case StreamProcess.ExitCode(code) => Process.emit(code.right[Unit])
      case StreamProcess.OutLine(line) => Process.emit(line).to(sout).map(_.left[Int])
      case StreamProcess.ErrLine(line) => Process.emit(line).to(serr).map(_.left[Int])
    }

    val propagateErrCode = justCode.pipe(process1.fold(-1) { (acc, codeOrU) =>
      codeOrU match {
        case \/-(code) => code
        case -\/(_) => acc
      }
    }).last

    val codeValidOpt: Task[Option[Int]] =
      for {
        codeOpt <- propagateErrCode.runLast
      } yield {
        //peek inside option
        for {
          code <- codeOpt
          //if code is -1 then fold's zero was returned
          validOpt <- if (code == -1) None else code.some
        } yield validOpt
      }

    codeValidOpt.flatMap {
      case Some(code) => Task.now(code)
      case None => Task.fail(new UnsupportedOperationException("Process did not return exit code")) //actually should never happen
    }
  }
}

