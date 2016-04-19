package org.scalafmt.util

import scala.tools.nsc.Global
import scala.tools.nsc.Settings

/**
  * Borrowed from
  * https://github.com/lihaoyi/fastparse/blob/0d67eca8f9264bfaff68e5cbb227045ceac4a15f/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala
  */
object ScalacParser {
  var current = Thread.currentThread().getContextClassLoader
  val files = collection.mutable.Buffer.empty[java.io.File]
  val settings = new Settings()
  files.appendAll(
      System
        .getProperty("sun.boot.class.path")
        .split(":")
        .map(new java.io.File(_)))
  while (current != null) {
    current match {
      case t: java.net.URLClassLoader =>
        files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
      case _ =>
    }
    current = current.getParent
  }
  val global = new Global(settings)
  settings.usejavacp.value = true
  settings.embeddedDefaults[ScalacParser.type]
  settings.classpath.append(files.mkString(":"))

  def checkParseFails(input: String) = this.synchronized {
    val run = new global.Run()
    var fail = false
    import global.syntaxAnalyzer.Offset
    val cu = new global.CompilationUnit(global.newSourceFile(input))
    val parser = new global.syntaxAnalyzer.UnitParser(cu, Nil) {

      override def newScanner() =
        new global.syntaxAnalyzer.UnitScanner(cu, Nil) {

          override def error(off: Offset, msg: String) = {
            fail = true
          }

          override def syntaxError(off: Offset, msg: String) = {
            fail = true
          }

          override def incompleteInputError(off: Offset, msg: String) = {
            fail = true
          }
        }

      override def incompleteInputError(msg: String) = {
        fail = true
      }

      override def syntaxError(offset: Offset, msg: String) = {
        fail = true
      }
    }
    parser.parse()
    fail
  }
}
