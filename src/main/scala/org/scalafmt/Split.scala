package org.scalafmt

sealed abstract class Split(val cost: Int,
                            val indent: Int,
                            val Strategy: Strategy = EmptyStrategy) {
  def length: Int = this match {
    case _: NoSplit => 0
    case _: Newline => 0
    case _: Space => 1
  }
}

// Direct subclasses.

case class NoSplit(override val cost: Int) extends Split(cost, 0)

case class Space(override val cost: Int,
                 override val Strategy: Strategy = EmptyStrategy)
  extends Split(cost: Int, 0, Strategy)

case class Newline(override val cost: Int,
                   override val indent: Int,
                   override val Strategy: Strategy = EmptyStrategy)
  extends Split(cost, indent, Strategy)

// objects

object NoSplitFree extends NoSplit(0)

object SpaceFree extends Space(0)

class NewlineFree(indent: Int) extends Newline(0, indent, EmptyStrategy)

object Newline0 extends NewlineFree(0)

object Newline_2 extends NewlineFree(-2)

object Newline2 extends NewlineFree(2)

object Newline_4 extends NewlineFree(-4)

object Newline4 extends NewlineFree(4)




