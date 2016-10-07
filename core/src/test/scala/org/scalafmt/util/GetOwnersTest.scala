package org.scalafmt.util

import scala.meta.dialects.Scala211
import scala.meta.internal.parsers.ScalametaParser

import org.scalafmt.UnitTests
import org.scalafmt.util.TokenOps.TokenHash
import org.scalatest.FunSuite

class GetOwnersTest extends FunSuite with HasTests {
  def check(diffTest: DiffTest): Unit = {
    test(diffTest.fullName) {
      import scala.meta._
      val input = Input.String(diffTest.original)
      val dialect = Scala211
      val tree: Tree =
        Scala211(input).parse(filename2parse(diffTest.filename).get).get

      val parser = new ScalametaParser(input, Scala211)
      val pos = new parser.TreePos(tree)
      val hash2tok: Map[TokenHash, Token] =
        tree.tokens.map(x => TokenOps.hash(x) -> x).toMap
      val slow = TreeOps.getOwners(tree)
      val fast = TreeOps.fastGetOwners(tree)
      val sslow = slow.map {
        case (k, v) => hash2tok(k).structure -> v
      }
      val ffast = fast.map {
        case (k, v) => hash2tok(k).structure -> v
      }
//      logger.elem(sslow, ffast)
//      tree.tokens.foreach(tok => logger.elem(tok.structure))
      tree.tokens.foreach {
        case Whitespace() =>
        case tok if tok.start == tok.end =>
        case tok =>
          val k = TokenOps.hash(tok)
          val (l, r) = fast(k) -> slow(k)
          if (l != r) {
            logger.elem(tok, tok.structure, l, l.structure, r, r.structure)
            ???
          }
      }
    }
  }
  UnitTests.tests.withFilter(!_.skip).foreach(check)

  override def tests: Seq[DiffTest] = Nil
}
