package org.scalafmt

/**
  * A state represents one potential solution to reach token at index,
  *
  * @param cost
  * @param strategy
  * @param path
  * @param indentation
  * @param column
  */
case class State(cost: Int,
                 strategy: PartialFunction[FormatToken, List[Split]],
                 path: Vector[Split],
                 indentation: Int,
                 column: Int) extends Ordered[State] {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.path.length) compare(-that.cost, that.path.length)
}


object State {
  val start = State(0, EmptyStrategy, Vector.empty[Split], 0, 0)
}
