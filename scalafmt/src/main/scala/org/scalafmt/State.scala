package org.scalafmt


/**
  * A state represents one potential solution to reach token at index,
  * @param cost The penalty for using path
  * @param path The splits/decicions made to reach here.
  */
case class State(cost: Int,
                 path: Vector[Split]) extends Ordered[State] {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.path.length) compare(-that.cost, that.path.length)
}

