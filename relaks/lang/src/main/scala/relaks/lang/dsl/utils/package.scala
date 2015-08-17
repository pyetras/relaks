package relaks.lang.dsl

import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Cause.EarlyCause

/**
 * Created by Pietras on 23/05/15.
 */
package object utils {
  type TupleLU[H <: HList, LU] = ToTraversable.Aux[H, List, LU] //TODO: make it more efficient and preserve classtag

  trait NamesToStrings[H <: HList] {
    def apply(h: H): Vector[String]
  }

  object NamesToStrings {
    def apply[H <: HList](implicit names: NamesToStrings[H]) = names
    implicit def symbolNamesToStrings[H <: HList](implicit namesT: ToTraversable.Aux[H, Vector, Symbol]): NamesToStrings[H] =
      new NamesToStrings[H] {
        override def apply(h: H): Vector[String] = namesT.apply(h).map(_.name)
      }

    implicit def stringNamesToString[H <: HList](implicit namesT: ToTraversable.Aux[H, Vector, String]): NamesToStrings[H] =
      new NamesToStrings[H] {
        override def apply(h: H): Vector[String] = namesT.apply(h)
      }
  }

  import scalaz.stream._
  //https://github.com/scalaz/scalaz-stream/issues/323
  trait Stepper[F[_], A] {
    def next: OptionT[F, Seq[A]]
    def close: F[Unit]
  }

  implicit class ProcessIterator[F[_], T](val p: Process.type) extends AnyVal {
    import p._
    def step[A](p: Process[Task, A]): Stepper[Task, A] = new Stepper[Task, A] {
      var state = p

      def next: OptionT[Task, Seq[A]] = state.step match {

        case Halt(_) => OptionT.none

        case Step(Emit(as: Seq[A]), cont) =>
          state = cont.continue
          // 1st change
          OptionT(as.point[Task] map some)

        case Step(Await(req: Task[_] @unchecked, rcv), cont) =>
          for {
            tail <- (req.attempt map { r => rcv(EarlyCause fromTaskResult r).run +: cont }).liftM[OptionT]
            _ = state = tail          // purity ftw!
            // 2nd change s/step/next
            back <- next
          } yield back
      }
      // 3rd change
      def close =
        Task.suspend {
          Task.delay(state = state.kill) >>
            state.run
        }
    }

  }
}
