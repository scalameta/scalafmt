package org.scalafmt

sealed trait Indent

case object NoOp extends Indent

case object Pop extends Indent

case object PushStateColumn extends Indent

case class Push(num: Int) extends Indent {
  require(num > 0)
}
