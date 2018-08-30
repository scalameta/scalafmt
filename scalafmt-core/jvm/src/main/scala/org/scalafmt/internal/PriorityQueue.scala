package org.scalafmt.internal

import java.util.Comparator

/**
  * Minimal implementation of the PriorityQueue's functions needed.
  *
  * We use [[java.util.PriorityQueue]] to enable usage under GraalVM. The
  * native-image compiler is unable to work with
  * [[scala.collection.mutable.PriorityQueue]] currently.
  *
  * @tparam T the values inside the queue
  */
class PriorityQueue[T <: Ordered[T]] {
  private[this] val q =
    new java.util.PriorityQueue[T](11, new Comparator[T] {
      override def compare(t: T, t1: T): Int = t1.compare(t)
    })

  def dequeueAll: Unit = q.clear()

  def dequeue(): T = q.poll()

  def size: Int = q.size()

  def enqueue(x: T): Unit = q.add(x)

  def +=(x: T): PriorityQueue[T] = {
    q.add(x)
    this
  }

  def nonEmpty: Boolean = !isEmpty

  def isEmpty: Boolean = q.isEmpty

}
