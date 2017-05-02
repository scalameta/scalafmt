package org.scalafmt.cli

import scopt.Read

sealed trait WriteMode

object WriteMode {
  implicit val readInst: Read[WriteMode] =
    Read.reads { _.toLowerCase match {
      case "override" => Override
      case "stdout" => Stdout
      case otherwise =>
        throw new IllegalArgumentException(
          s"`$otherwise` is not allowed. Expected `override` or `stdout`")
    }}
}

case object Override extends WriteMode
case object Stdout extends WriteMode
