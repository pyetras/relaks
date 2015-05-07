package relaks.optimizer

import scala.util.{Failure, Success, Try}
import scalaz.concurrent.Task
import scalaz.stream.{Process, io}
import scala.language.implicitConversions

/**
 * Created by Pietras on 07/05/15.
 */
package object util {
  object syntax {
    implicit def addStrProcOps(proc: Process[Task, StreamProcess.SPResult]): StrProcOps = new StrProcOps(proc)
    implicit def addIoOps(pkg: io.type): ScalazStreamIoOps = new ScalazStreamIoOps(pkg)
    implicit def tryToTask[A](tr: Try[A]): Task[A] = tr match {
      case Success(result) => Task.now(result)
      case Failure(e) => Task.fail(e)
    }
  }

}
