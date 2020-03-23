package org.scalafmt.internal

sealed abstract class SplitTag {

  def activateOnly(splits: Seq[Split]): Seq[Split]

}

object SplitTag {

  abstract class Base extends SplitTag {
    override final def activateOnly(splits: Seq[Split]): Seq[Split] = splits
  }

  abstract class Custom extends SplitTag {
    override final def activateOnly(splits: Seq[Split]): Seq[Split] =
      splits.map(_.activateFor(this)).filter(_.isActive)
  }

  case object Active extends Base
  case object Ignored extends Base

  case object OneArgPerLine extends Custom
  case object SelectChainFirstNL extends Custom

}
