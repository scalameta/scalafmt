package org.scalafmt.config

import metaconfig._

case class TreePatterns(patterns: Seq[TreePattern]) {
  def isEmpty: Boolean = patterns.isEmpty
  def matches(tree: meta.Tree): Boolean = matchers.exists(_.matches(tree))
  private lazy val matchers = patterns.distinct.map(_.getMatcher)
}

object TreePatterns {
  val empty = TreePatterns(Nil)

  implicit val encoder: ConfEncoder[TreePatterns] =
    implicitly[ConfEncoder[Seq[TreePattern]]].contramap(_.patterns)

  implicit val decoder: ConfDecoderEx[TreePatterns] = {
    val seqDecoder = implicitly[ConfDecoderEx[Seq[TreePattern]]]
    (s, c) => seqDecoder.read(s.map(_.patterns), c).map(TreePatterns.apply)
  }
}
