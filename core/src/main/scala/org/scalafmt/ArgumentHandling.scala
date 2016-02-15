package org.scalafmt

/**
  * How are arguments to term/type application handled.
  */
sealed trait ArgumentHandling

/**
  * Only allow splits after commas with indentation 4.
  * Arguments are not aligned together
  */
case object BinPacking extends ArgumentHandling

/**
  * If arguments overflow column limit, put each argument on separate line.
  * Tries first to align indentation with opening (, then at indentation 4.
  */
case object OneArgOneLine extends ArgumentHandling
