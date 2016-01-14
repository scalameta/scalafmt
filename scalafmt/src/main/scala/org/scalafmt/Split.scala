package org.scalafmt

trait Split

case object NoSplit extends Split

case object Space extends Split

case object Newline extends Split

