package org.scalafmt

/**
  * A state represents one potential solution to reach token at index,
  *
  * @param cost
  * @param path
  * @param indentation
  * @param column
  */
case class State(cost: Int,
                policy: Decision => Decision,
                 path: Vector[Split],
                 indentation: Int,
                 column: Int) extends Ordered[State] {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.path.length, -this.indentation) compare
      (-that.cost, that.path.length, -that.indentation)
}


object State {
  val start = State(0, identity, Vector.empty[Split], 0, 0)
}
