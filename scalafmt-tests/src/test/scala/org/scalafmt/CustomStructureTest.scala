package org.scalafmt

import munit.FunSuite

import scala.meta._
import scala.meta.internal.parsers.ScalametaParser

class CustomStructureTest extends FunSuite {

  private def check(
      original: String,
      expected: Tree,
      dialect: Dialect
  )(implicit loc: munit.Location): Unit = {
    val parser = new ScalametaParser(Input.String(original))(dialect)
    assertNoDiff(parser.parseStat().structure, expected.structure)
  }

  // #3634
  check(
    """
      |def foo(a: Foo[_]): Unit = ???
      """.stripMargin,
    Defn.Def(
      Nil,
      Term.Name("foo"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(
                Term.Param(
                  Nil,
                  Term.Name("a"),
                  Some(
                    Type.Apply(
                      Type.Name("Foo"),
                      Type.ArgClause(
                        List(Type.Wildcard(Type.Bounds(None, None)))
                      )
                    )
                  ),
                  None
                )
              ),
              None
            )
          )
        )
      ),
      Some(Type.Name("Unit")),
      Term.Name("???")
    ),
    dialects.Scala3
  )

}
