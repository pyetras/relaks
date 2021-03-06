package relaks.lang.impl

import shapeless.{HNil, HList}

import scalaz.stream._
import scalaz.concurrent._
import scalaz.Scalaz._
import relaks.lang.dsl.utils.ProcessIterator
/**
 * Created by Pietras on 12.08.15.
 */
trait Table {
  def toIterator: Iterator[Row]
}

trait UntypedTable extends Table

trait TypedTable[H <: HList] extends Table

class TableProcess(rows: Process[Task, Row]) extends UntypedTable with TypedTable[HNil] {
  class TableIterator extends Iterator[Row] {
    lazy private val stepper = {
      Process.step(rows)
    }

    var current = none[Seq[Row]]
    var closed = false

    private def update() = {
      closed ? false | {
        current = stepper.next.run.run
        closed = true
        hasNext
      }
    }

    override def hasNext: Boolean = {
      current match {
        case None => update()
        case Some(Seq()) => update()
        case Some(a +: _) =>
          closed = false
          true
      }
    }

    override def next(): Row = hasNext ? {
      val Some(a +: rest) = current
      current = Some(rest)
      a
    } | null
  }


  def toIterator: Iterator[Row] = new TableIterator

}
