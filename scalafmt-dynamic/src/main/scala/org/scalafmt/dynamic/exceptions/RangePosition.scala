package org.scalafmt.dynamic.exceptions

case class RangePosition(
    start: Int,
    startLine: Int,
    startCharacter: Int,
    end: Int,
    endLine: Int,
    endCharacter: Int
)
