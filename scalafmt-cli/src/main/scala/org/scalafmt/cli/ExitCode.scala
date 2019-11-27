package org.scalafmt.cli

import scala.collection.mutable

sealed abstract case class ExitCode(code: Int, name: String) {
  def isOk: Boolean = this == ExitCode.Ok
  def is(c: ExitCode): Boolean =
    (code & c.code) != 0
  override def toString: String = s"$name=$code"
}

object ExitCode {
  // NOTE: ExitCode resembles an Enumeration very much, but has minor differences
  // for example how the name is calculated for merged exit codes.
  private var counter = 0
  private val allInternal = mutable.ListBuffer.empty[ExitCode]
  private val cache =
    new java.util.concurrent.ConcurrentHashMap[Int, ExitCode]
  private def generateExitStatus(implicit name: sourcecode.Name) = {
    val code = counter
    counter = if (counter == 0) 1 else counter << 1
    val result = new ExitCode(code, name.value) {}
    allInternal += result
    result
  }
  // see https://github.com/scalameta/scalafmt/issues/941
  // format: off
  val Ok,
      TestError,
      ParseError,
      CommandLineArgumentError,
      UnexpectedError,
      UnsupportedVersion
    : ExitCode = generateExitStatus
  // format: on
  lazy val all: List[ExitCode] = allInternal.toList
  private def codeToName(code: Int): String = {
    if (code == 0) Ok.name
    else {
      val names = all.collect {
        case exit if (exit.code & code) != 0 => exit.name
      }
      names.mkString("+")
    }
  }
  def apply(code: Int): ExitCode = {
    if (cache.contains(code)) cache.get(code)
    else {
      val result = new ExitCode(code, codeToName(code)) {}
      cache.put(code, result)
      result
    }
  }

  def merge(exit1: ExitCode, exit2: ExitCode): ExitCode =
    apply(exit1.code | exit2.code)
}
