package relaks.optimizer

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
  }

}
