package org.scalafmt
package jsfacade

import scala.scalajs.js
import js.JSConverters._
import js.annotation._

object JSFacade {

  @JSExportTopLevel("format")
  def format(input: String): String = {
    Scalafmt.format(input).get
  }

}
