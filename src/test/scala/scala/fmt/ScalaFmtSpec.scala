package scala.fmt

import org.scalatest.FunSpec

import scala.meta._

class ScalaFmtSpec extends FunSpec {

  def code(withUnit: Boolean): String = {
    val returnType = if (withUnit) ": Unit =" else ""
    s"""
object Foo {
  val unaffected: Unit = Unit
  val unaffected = 1
  def invalid$returnType {
    println("Hello scala.meta!")
  }
  def invalidWithComment(x: Int)$returnType { // foobar
    println("Hello scala.meta!")
  }
  def invalidWithParams(x: Int)$returnType {
    println("Hello scala.meta!")
  }
  @withMods def invalidWithParams(x: Int)$returnType {
    println("Hello scala.meta!")
  }
  def valid: Unit = {
    println("Hello scala.meta!")
  }
}"""
// Only failing example:
//  def invalidWithComment(x: Int)$returnType /* pathological */ {
//    println("Hello scala.meta!")
//  }
  }
  describe("procedure syntax") {
    it("works in some basic cases"){
      val expected = code(withUnit = true).parse[Source]
      val original = code(withUnit = false).parse[Source]
      val result = ScalaFmt.procedureSyntax(original)
      assert(expected.show[Syntax] === result.show[Syntax])
    }
  }
}

