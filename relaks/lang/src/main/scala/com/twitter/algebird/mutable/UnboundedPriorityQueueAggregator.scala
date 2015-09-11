package com.twitter.algebird.mutable

import java.util.PriorityQueue
import scala.collection.JavaConverters._
import com.twitter.algebird.{Monoid, MonoidAggregator}

/**
 * Created by Pietras on 19.08.15.
 */
class UnboundedPriorityQueueAggregator[A](implicit ord: Ordering[A])
  extends MonoidAggregator[A, PriorityQueue[A], Seq[A]] {
  override val monoid = new InfPriorityQueueMonoid[A]

  override def present(reduction: PriorityQueue[A]) = new Iterator[A] {
    override def hasNext: Boolean = !reduction.isEmpty
    override def next(): A = reduction.poll()
  }.toSeq

  override def prepare(input: A): PriorityQueue[A] = monoid.build(input)
}
