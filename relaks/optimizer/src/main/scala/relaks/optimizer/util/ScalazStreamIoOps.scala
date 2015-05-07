package relaks.optimizer.util

import java.nio.ByteBuffer
import java.nio.channels.{CompletionHandler, AsynchronousFileChannel}
import java.nio.file.{StandardOpenOption, Path}

import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.{Channel, io}

/**
 * Created by Pietras on 07/05/15.
 */
class ScalazStreamIoOps(val self: io.type) extends AnyVal {
  def asyncFileLinesW(path: Path): Channel[Task, String, Unit] = self.resource {
    Task.delay(AsynchronousFileChannel.open(path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE))
  } { (ch: AsynchronousFileChannel) =>
    Task.delay(ch.close())
  } { (ch: AsynchronousFileChannel) =>
    Task.now { (line: String) => //                                               v- trololololo
      Task.async(cb => ch.write(ByteBuffer.wrap((line + "\n").getBytes("UTF-8")), 0, (), new CompletionHandler[Integer, Unit] {
        override def completed(result: Integer, attachment: Unit): Unit = cb(\/-(()))
        override def failed(exc: Throwable, attachment: Unit): Unit = cb(-\/(exc))
      }))
    }
  }

//  def asyncFileLinesR(path: Path): Process[Task, String] = self.resource {
//    Task.delay(AsynchronousFileChannel.open(path, StandardOpenOption.READ))
//  } { ch =>
//    Task.delay(ch.close())
//  } { (ch: AsynchronousFileChannel) =>
//    ch.read()
//  }
}
