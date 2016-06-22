package org.scalafmt

import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.State
import org.scalafmt.internal.Split

/**
  * An event that happens while formatting a file.
  */
abstract class FormatEvent

object FormatEvent {
  case class CreateFormatOps(formatOps: FormatOps) extends FormatEvent
  case class VisitToken(formatToken: FormatToken) extends FormatEvent
  case class Explored(n: Int, depth: Int, queueSize: Int) extends FormatEvent
  case class Enqueue(split: Split) extends FormatEvent
  case class CompleteFormat(totalExplored: Int,
                            finalState: State,
                            tokens: Array[FormatToken])
      extends FormatEvent
}
