package org.scalafmt.readme

import org.scalafmt.Scalafmt
import org.scalafmt.Versions
import org.scalafmt.cli.Cli
import org.scalafmt.cli.CliArgParser
import org.scalafmt.config.Config
import org.scalafmt.config._
import org.scalafmt.rewrite._

import java.text.SimpleDateFormat
import java.util.Date

import com.twitter.util.Eval

import metaconfig.Conf
import metaconfig.ConfEncoder
import metaconfig.generic.Settings
import metaconfig.generic.Surface
import scalatags.Text.TypedTag
import scalatags.Text.all._

object hl extends scalatex.site.Highlighter

object Readme {

  def gitter = a(
    href := "https://gitter.im/scalameta/scalafmt",
    img(
      src :=
        "https://camo.githubusercontent.com/da2edb525cde1455a622c58c0effc3a90b9a181c/68747470733a2f2f6261646765732e6769747465722e696d2f4a6f696e253230436861742e737667",
      alt := "Join the chat at https://gitter.im/scalameta/scalameta",
      maxWidth := "100%;",
    ),
  )

  val eval = new Eval()
  implicit def bool2frag(boolean: Boolean): StringFrag =
    stringFrag(boolean.toString)

  def flag(str: String) = {
    println(str)
    Config.fromHoconString(str).get
    code(str)
  }

  /** repl session, inspired by tut.
    *
    * Example: {{{code="1 + 1"}}} returns
    * {{{
    *   scala> 1 + 1
    *   res0: Int = 2
    * }}}
    */
  def repl(code: String) = {
    import scala.meta._
    val expressions = s"{$code}".parse[Stat].get.asInstanceOf[Term.Block].stats
    val evaluated = eval[Any](code)
    val output = evaluated match {
      case s: String =>
        s"""|
            |"$s"""".stripMargin
      case x => x.toString
    }
    val result = s"""|${expressions.map(x => s"scala> ${x.toString().trim}")
                      .mkString("\n")}
                     |res0: ${evaluated.getClass.getName} = $output
                     |""".stripMargin
    hl.scala(result)
  }

  def config(frags: Frag*) = cliFlags(frags.render)
  def cliFlags(flags: String) = {
    Config.fromHoconString(flags).get
    hl.scala(flags)
  }

  def note = b("NOTE")

  def warning(frags: Frag*) = div(frags, `class` := "warning")

  def sidenote(frags: Frag*) = div(frags, `class` := "sidenote")

  def github: String = "https://github.com"
  def repo: String = "https://github.com/scalameta/scalafmt"
  def gitRepo: String = repo + ".git"

  def user(name: String) = a(href := s"$github/$name", s"@$name")
  def users(names: String*) = span(
    names.dropRight(1).map(x => span(user(x), ", ")) :+ user(names.last): _*,
  )

  def pr(id: Int) = a(href := repo + s"/pull/$id", s"#$id")
  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")
  def issues(ids: Int*) = span(ids.map(issue): _*)

  def half(frags: Frag*) = div(frags)
  def ignore(frags: Frag*) = span("")

  def pairs(frags: Frag*) = div(frags, `class` := "scalafmt-pair")

  def rows(frags: Frag*) = div(frags, `class` := "scalafmt-rows")

  def sideBySide(left: String, right: String): TypedTag[String] =
    pairs(List(left, right).map(x => half(hl.scala(x))): _*)

  def demo(code: String) = {
    val formatted = Scalafmt.format(code, ScalafmtConfig.default40).get
    sideBySide(code, formatted)
  }

  def changedConfig(style: ScalafmtConfig): String = {
    val diff = Conf.patch(
      ConfEncoder[ScalafmtConfig].write(ScalafmtConfig.default),
      ConfEncoder[ScalafmtConfig].write(style),
    )
    Conf.printHocon(diff)
  }

  def allOptions = hl.scala.apply(Conf.printHocon(ScalafmtConfig.default))

  def configurationBlock(
      style: ScalafmtConfig,
      collapsed: Boolean = false,
  ): TypedTag[String] = div(
    span(
      "Show/hide configuration used for this example",
      `class` := "scalafmt-configuration-toggle",
    ),
    pre(changedConfig(style)),
    `class` := "scalafmt-configuration" + (if (collapsed) " collapsed" else ""),
  )

  def fullWidthDemo(style: ScalafmtConfig)(code: String): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style).get
    div(
      rows(List(
        div(hl.scala(code), `class` := "before"),
        div(hl.scala(formatted), `class` := "after"),
      )),
      configurationBlock(style),
    )
  }

  def demoStyle(style: ScalafmtConfig)(code: String): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style.copy(runner = ScalafmtRunner.sbt))
      .get
    div(sideBySide(code, formatted), configurationBlock(style))
  }

  def example(code: String): TypedTag[String] =
    example(code, ScalafmtConfig.default40)

  def exampleAlign(code: String): TypedTag[String] = {
    val formatted = Scalafmt.format(
      code,
      ScalafmtConfig.default40.copy(align =
        ScalafmtConfig.default40.align.copy(tokens = AlignToken.default),
      ),
    ).get
    hl.scala(formatted)
  }

  val stripMarginStyle = ScalafmtConfig.default
    .copy(assumeStandardLibraryStripMargin = true)

  val rewriteInfix = ScalafmtConfig.default
    .copy(rewrite = ScalafmtConfig.default.rewrite.copy(rules = Seq(AvoidInfix)))

  val rewriteImportSelectors = ScalafmtConfig.default.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(rules = Seq(ExpandImportSelectors)),
  )

  val rewriteBraces = ScalafmtConfig.default.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(
      redundantBraces = ScalafmtConfig.default.rewrite.redundantBraces
        .copy(stringInterpolation = true),
      rules = Seq(RedundantBraces),
    ),
  )

  val rewriteParens = ScalafmtConfig.default.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(rules = Seq(RedundantParens)),
  )

  val rewriteImports = ScalafmtConfig.default
    .copy(rewrite = ScalafmtConfig.default.rewrite.copy(rules = Seq(SortImports)))

  val rewriteAsciiImports = ScalafmtConfig.default.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(rules = Seq(AsciiSortImports)),
  )

  val rewriteSortModifiers = ScalafmtConfig.default120.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(rules = Seq(SortModifiers)),
  )

  /** This looks way too hacky. But can't seem to find a typeclass that ought to
    * "encode" the ``ModKey`` enum.
    *
    * Additionally, a Vector of Strings is simply concatenated, hence the extra
    * .mkString.
    * {{{
    *     [error]  found   : Vector[org.scalafmt.config.SortSettings.ModKey]
    *     [error]  required: scalatags.Text.Modifier
    *     [error]     (which expands to)  scalatags.generic.Modifier[scalatags.text.Builder]
    *     [error]           @code{rewrite.sortModifiers.order} = @rewriteSortModifiers.rewrite.sortModifiers.order
    * }}}
    */
  val rewriteSortModifiersDefaultString = SortSettings.defaultOrder
    .map(_.productPrefix).mkString("[\"", "\", \"", "\"]")

  val rewritePreferCurlyFors = ScalafmtConfig.default.copy(rewrite =
    ScalafmtConfig.default.rewrite.copy(rules = Seq(PreferCurlyFors)),
  )

  val verticalAlign = ScalafmtConfig.default.copy(
    maxColumn = 60,
    verticalMultiline = VerticalMultiline(atDefnSite = true),
  )

  val verticalMultilineDefaultConfigStr = Conf.printHocon(Conf.Obj(
    "verticalMultiline" ->
      ConfEncoder[VerticalMultiline]
        .write(ScalafmtConfig.default.verticalMultiline),
  ))

  val verticalAlignImplicitBefore = ScalafmtConfig.default.copy(
    maxColumn = 60,
    verticalMultiline =
      VerticalMultiline(atDefnSite = true, newlineBeforeImplicitKW = true),
  )

  val verticalAlignImplicitAfter = ScalafmtConfig.default.copy(
    maxColumn = 60,
    verticalMultiline =
      VerticalMultiline(atDefnSite = true, newlineAfterImplicitKW = true),
  )

  val multilineNewlineAfterParen = ScalafmtConfig.default.copy(
    indent = ScalafmtConfig.default.indent.copy(defnSite = 2),
    verticalMultiline = VerticalMultiline(
      atDefnSite = true,
      arityThreshold = 2,
      newlineAfterOpenParen = true,
    ),
  )

  val multilineDanglingParens = ScalafmtConfig.default.copy(
    indent = ScalafmtConfig.default.indent.copy(defnSite = 2),
    verticalMultiline = VerticalMultiline(
      atDefnSite = true,
      arityThreshold = 2,
      excludeDanglingParens = Nil,
    ),
  )

  val newlineAlwaysBeforeTopLevelStatements = ScalafmtConfig.default
    .copy(newlines =
      ScalafmtConfig.default.newlines.copy(alwaysBeforeTopLevelStatements = true),
    )

  val arityThreshold = ScalafmtConfig.default.copy(verticalMultiline =
    VerticalMultiline(atDefnSite = true, arityThreshold = 2),
  )

  def fmt(style: ScalafmtConfig)(code: String): TypedTag[String] =
    example(code, style)

  def lastUpdated = new SimpleDateFormat("MMM d, y")
    .format(new Date(Versions.timestamp.toLong))

  def format(code: String): TypedTag[String] =
    format(ScalafmtConfig.default)(code)

  val alignNone = ScalafmtConfig.default.copy(align = Align.none)
  val alignSome = ScalafmtConfig.default.copy(align = Align.some)
  val alignMore = ScalafmtConfig.default.copy(align = Align.more)
  val alignMost = ScalafmtConfig.default.copy(align = Align.most)
  val alignCaseArrow = ScalafmtConfig.default
  val alignArrowEnum = ScalafmtConfig.defaultWithAlign
    .copy(align = Align.default.copy(arrowEnumeratorGenerator = true))
  val alignModuleId = ScalafmtConfig.defaultWithAlign

  def format(style: ScalafmtConfig)(code: String): TypedTag[String] =
    example(code, style.copy(runner = ScalafmtRunner.sbt))

  def example(code: String, style: ScalafmtConfig): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style).get
    hl.scala(formatted)
  }
  def image(url: String) =
    img(src := url, width := "100%", textAlign := "center")
}
