package org.scalafmt.util

sealed abstract class LogLevel(color: String)(implicit name: sourcecode.Name) {

  override def toString: String = s"[$color${name.value}${Console.RESET}]"
}

object LogLevel {

  case object Trace extends LogLevel(Console.RESET)

  case object Debug extends LogLevel(Console.GREEN)

  case object Info extends LogLevel(Console.BLUE)

  case object Warn extends LogLevel(Console.YELLOW)

  case object Error extends LogLevel(Console.RED)
}

