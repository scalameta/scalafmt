package org.scalafmt

import scala.concurrent.duration.Duration

/**
  * A scalafmt style, use [[ScalaStyle.Standard]].
  */
// TODO(olafur) refactor to case class or something simpler
sealed trait ScalaStyle {
  /**
    * Column limit, any formatting exceeding this field is penalized heavily.
    */
  // TODO(olafur) soft column-limit?
  def maxColumn: Int = 80

  /**
    * The maximum duration the formatter is allowed to run before giving up.
    * If the formatter times out, the original source code is returned.
    */
  def maxDuration: Duration = Duration(400, "ms")

  /**
    * Debugging only. Should scalafmt create a diagnostics report.
    */
  def debug: Boolean = false

}

/**
  * Debugging only. Used in unit tests.
  */
protected[scalafmt] sealed trait UnitTestStyle extends ScalaStyle {
  override val debug = true

  override def maxDuration = Duration(10, "s")
}

object ScalaStyle {

  /**
    * Recommended style if you are not sure which one to pick.
    */
  case object Standard extends ScalaStyle

  protected[scalafmt] case object ManualTest extends ScalaStyle {
    override val debug = true

    override def maxDuration = Duration(10, "min")
  }

  protected[scalafmt] case object UnitTest80 extends UnitTestStyle

  protected[scalafmt] case object UnitTest40 extends UnitTestStyle {
    override def maxColumn = 40
  }

}

