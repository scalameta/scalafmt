package org.scalafmt.internal

sealed abstract class SplitTag {

  final def activateOnly(splits: Seq[Split]): Seq[Split] =
    splits.map(_.activateFor(this))

}

object SplitTag {

  case object OneArgPerLine extends SplitTag
  case object SelectChainFirstNL extends SplitTag
  case object SelectChainSecondNL extends SplitTag
  case object InfixChainNoNL extends SplitTag
  case object OnelineWithChain extends SplitTag
  case object VerticalMultilineSingleLine extends SplitTag

}
