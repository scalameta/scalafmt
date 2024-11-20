package org.scalafmt.config

import org.scalafmt.internal._

/** An event that happens while formatting a file.
  */
sealed abstract class FormatEvent

object FormatEvent {
  case class CreateFormatOps(formatOps: FormatOps) extends FormatEvent
  case class Routes(routes: IndexedSeq[Seq[Split]]) extends FormatEvent
  case class VisitToken(formatToken: FT) extends FormatEvent
  case class Explored(n: Int, depth: Int, queueSize: Int) extends FormatEvent
  case class Enqueue(split: Split) extends FormatEvent
  case class CompleteFormat(
      totalExplored: Int,
      finalState: State,
      visits: IndexedSeq[Int],
      best: collection.Map[Int, State],
  ) extends FormatEvent
  case class Written(formatLocations: FormatWriter#FormatLocations)
      extends FormatEvent
}
