package org.scalafmt

import scala.meta.Tree
import scala.meta.parsers.Parse

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.HasTests
import org.scalafmt.util.Report
import org.scalatest.BeforeAndAfterAll
import org.scalatest.ConfigMap
import org.scalatest.FunSuite

class BestEffortTest
    extends FunSuite
    with HasTests
    with DiffAssertions
    with BeforeAndAfterAll {
  // Don't be afraid to override the expected value in these tests.
  val stateExplosions =
    """
<<< scala.meta tree structure
Defn.Object(Nil, Term.Name("State"), Template(Nil, Seq(Ctor.Ref.Name("ScalaFmtLogger")), Term.Param(Nil, Name.Anonymous(), None, None), Some(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("start"))), None, Term.Apply(Term.Name("State"), Seq(Lit(0), Term.Name("identity"), Term.ApplyType(Term.Select(Term.Name("Vector"), Term.Name("empty")), Seq(Type.Name("Split"))), Term.ApplyType(Term.Select(Term.Name("Vector"), Term.Name("empty")), Seq(Type.Name("State"))), Lit(0), Term.ApplyType(Term.Select(Term.Name("Vector"), Term.Name("empty")), Seq(Type.Apply(Type.Name("Indent"), Seq(Type.Name("Num"))))), Lit(0)))), Defn.Def(Nil, Term.Name("reconstructPath"), Nil, Seq(Seq(Term.Param(Nil, Term.Name("toks"), Some(Type.Apply(Type.Name("Array"), Seq(Type.Name("FormatToken")))), None), Term.Param(Nil, Term.Name("splits"), Some(Type.Apply(Type.Name("Vector"), Seq(Type.Name("Split")))), None), Term.Param(Nil, Term.Name("style"), Some(Type.Name("ScalaStyle")), None), Term.Param(Nil, Term.Name("debug"), Some(Type.Name("Boolean")), Some(Lit(false))))), Some(Type.Apply(Type.Name("Seq"), Seq(Type.Tuple(Seq(Type.Name("FormatToken"), Type.Name("String")))))), Term.Block(Seq(Defn.Var(Nil, Seq(Pat.Var.Term(Term.Name("state"))), None, Some(Term.Select(Term.Name("State"), Term.Name("start")))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("result"))), None, Term.Apply(Term.Select(Term.Apply(Term.Select(Term.Name("toks"), Term.Name("zip")), Seq(Term.Name("splits"))), Term.Name("map")), Seq(Term.PartialFunction(Seq(Case(Pat.Tuple(Seq(Pat.Var.Term(Term.Name("tok")), Pat.Var.Term(Term.Name("split")))), None, Term.Block(Seq(Term.If(Term.Name("debug"), Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("left"))), None, Term.Apply(Term.Name("small"), Seq(Term.Select(Term.Name("tok"), Term.Name("left"))))), Term.Apply(Term.Select(Term.Name("logger"), Term.Name("debug")), Seq(Term.Interpolate(Term.Name("f"), Seq(Lit(""), Lit("%-10s "), Lit("")), Seq(Term.Name("left"), Term.Name("split"))))))), Lit()), Term.Assign(Term.Name("state"), Term.Apply(Term.Select(Term.Name("state"), Term.Name("next")), Seq(Term.Name("style"), Term.Name("split"), Term.Name("tok")))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("whitespace"))), None, Term.Match(Term.Select(Term.Name("split"), Term.Name("modification")), Seq(Case(Term.Name("Space"), None, Lit(" ")), Case(Pat.Typed(Pat.Var.Term(Term.Name("nl")), Type.Name("NewlineT")), None, Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("newline"))), None, Term.If(Term.Select(Term.Name("nl"), Term.Name("isDouble")), Lit("\n\n"), Lit("\n"))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("indentation"))), None, Term.If(Term.Select(Term.Name("nl"), Term.Name("noIndent")), Lit(""), Term.ApplyInfix(Lit(" "), Term.Name("*"), Nil, Seq(Term.Select(Term.Name("state"), Term.Name("indentation")))))), Term.ApplyInfix(Term.Name("newline"), Term.Name("+"), Nil, Seq(Term.Name("indentation")))))), Case(Pat.Extract(Term.Name("Provided"), Nil, Seq(Pat.Var.Term(Term.Name("literal")))), None, Term.Name("literal")), Case(Term.Name("NoSplit"), None, Lit(""))))), Term.ApplyInfix(Term.Name("tok"), Term.Name("->"), Nil, Seq(Term.Name("whitespace"))))))))))), Term.If(Term.Name("debug"), Term.Apply(Term.Select(Term.Name("logger"), Term.Name("debug")), Seq(Term.Interpolate(Term.Name("s"), Seq(Lit("Total cost: "), Lit("")), Seq(Term.Select(Term.Name("state"), Term.Name("cost")))))), Lit()), Term.Name("result"))))))))
>>>
Defn.Object(
    Nil,
    Term.Name("State"),
    Template(
        Nil,
        Seq(Ctor.Ref.Name("ScalaFmtLogger")),
        Term.Param(Nil, Name.Anonymous(), None, None),
        Some(
            Seq(Defn.Val(Nil,
                         Seq(Pat.Var.Term(Term.Name("start"))),
                         None,
                         Term.Apply(Term.Name("State"),
                                    Seq(Lit(0),
                                        Term.Name("identity"),
                                        Term.ApplyType(
                                            Term.Select(Term.Name("Vector"), Term.Name("empty")),
                                            Seq(Type.Name("Split"))),
                                        Term
                                          .ApplyType(Term.Select(
                                                         Term.Name("Vector"),
                                                         Term.Name("empty")),
                                                     Seq(Type.Name("State"))),
                                        Lit(0),
                                        Term
                                          .ApplyType(Term
                                                       .Select(Term.Name(
                                                                   "Vector"),
                                                               Term.Name(
                                                                   "empty")),
                                                     Seq(
                                                         Type.Apply(
                                                             Type
                                                               .Name("Indent"),
                                                             Seq(Type.Name(
                                                                     "Num"))))),
                                        Lit(0)))),
                Defn.Def(Nil,
                         Term.Name("reconstructPath"),
                         Nil,
                         Seq(
                             Seq(Term.Param(Nil,
                                            Term.Name("toks"),
                                            Some(
                                                Type.Apply(Type.Name("Array"), Seq(Type.Name("FormatToken")))), None), Term.Param(Nil, Term.Name("splits"), Some(Type.Apply(Type.Name("Vector"), Seq(Type.Name("Split")))), None), Term.Param(Nil, Term.Name("style"), Some(Type.Name("ScalaStyle")), None), Term.Param(Nil, Term.Name("debug"), Some(Type.Name("Boolean")), Some(Lit(false))))), Some(Type.Apply(Type.Name("Seq"), Seq(Type.Tuple(Seq(Type.Name("FormatToken"), Type.Name("String")))))), Term.Block(Seq(Defn.Var(Nil, Seq(Pat.Var.Term(Term.Name("state"))), None, Some(Term.Select(Term.Name("State"), Term.Name("start")))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("result"))), None, Term.Apply(Term.Select(Term.Apply(Term.Select(Term.Name("toks"), Term.Name("zip")), Seq(Term.Name("splits"))), Term.Name("map")), Seq(Term.PartialFunction(Seq(Case(Pat.Tuple(Seq(Pat.Var.Term(Term.Name("tok")), Pat.Var.Term(Term.Name("split")))), None, Term.Block(Seq(Term.If(Term.Name("debug"), Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("left"))), None, Term.Apply(Term.Name("small"), Seq(Term.Select(Term.Name("tok"), Term.Name("left"))))), Term.Apply(Term.Select(Term.Name("logger"), Term.Name("debug")), Seq(Term.Interpolate(Term.Name("f"), Seq(Lit(""), Lit("%-10s "), Lit("")), Seq(Term.Name("left"), Term.Name("split"))))))), Lit()), Term.Assign(Term.Name("state"), Term.Apply(Term.Select(Term.Name("state"), Term.Name("next")), Seq(Term.Name("style"), Term.Name("split"), Term.Name("tok")))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("whitespace"))), None, Term.Match(Term.Select(Term.Name("split"), Term.Name("modification")), Seq(Case(Term.Name("Space"), None, Lit(" ")), Case(Pat.Typed(Pat.Var.Term(Term.Name("nl")), Type.Name("NewlineT")), None, Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("newline"))), None, Term.If(Term.Select(Term.Name("nl"), Term.Name("isDouble")), Lit("\n\n"), Lit("\n"))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("indentation"))), None, Term.If(Term.Select(Term.Name("nl"), Term.Name("noIndent")), Lit(""), Term.ApplyInfix(Lit(" "), Term.Name("*"), Nil, Seq(Term.Select(Term.Name("state"), Term.Name("indentation")))))), Term.ApplyInfix(Term.Name("newline"), Term.Name("+"), Nil, Seq(Term.Name("indentation")))))), Case(Pat.Extract(Term.Name("Provided"), Nil, Seq(Pat.Var.Term(Term.Name("literal")))), None, Term.Name("literal")), Case(Term.Name("NoSplit"), None, Lit(""))))), Term.ApplyInfix(Term.Name("tok"), Term.Name("->"), Nil, Seq(Term.Name("whitespace"))))))))))), Term.If(Term.Name("debug"), Term.Apply(Term.Select(Term.Name("logger"), Term.Name("debug")), Seq(Term.Interpolate(Term.Name("s"), Seq(Lit("Total cost: "), Lit("")), Seq(Term.Select(Term.Name("state"), Term.Name("cost")))))), Lit()), Term.Name("result"))))))))

    """.replace("'''", "\"\"\"")

  override val tests =
    parseDiffTests(stateExplosions, "default/StateExplosion.stat")

  def run(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    val runner = scalafmtRunner.copy(parser = parse)
    val obtained = Scalafmt
      .format(
        t.original,
        t.style.copy(
          runner = runner,
          bestEffortInDeeplyNestedCode = true
        )
      )
      .get
    debugResults += saveResult(t, obtained, t.only)
    // Disabled, these tests change too often. This feature will be supported
    // with a best-effort :)
//    assertFormatPreservesAst(t.original, obtained)(parse)
//    assertNoDiff(obtained, t.expected)
  }

  testsToRun.foreach(runTest(run))

  override def afterAll(configMap: ConfigMap): Unit = {
    FileOps
      .writeFile("target/index.html", Report.heatmap(debugResults.result()))
  }
}
