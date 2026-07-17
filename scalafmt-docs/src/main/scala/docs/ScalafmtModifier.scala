package docs

import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig

import scala.meta.inputs.{Input, Position}
import scala.meta.parsers.ParseException

import mdoc.{Reporter, StringModifier}

class ScalafmtModifier extends StringModifier {

  import ScalafmtModifier._

  private final val separator = "---\n"

  override val name: String = "scalafmt"
  override def process(info: String, input: Input, reporter: Reporter): String = {
    val code = input.text
    val base = if (code.contains("package")) defaultConfig else defaultSbtConfig
    val i = code.indexOf(separator)
    def pos = Position.Range(input, 0, 0)
    if (i == -1) {
      reporter.error(pos, "Missing ---")
      "fail"
    } else {
      val config = code.substring(0, i)
      ScalafmtConfig.fromHoconString(config, base).fold { e =>
        reporter.error(pos, e.toString())
        "fail"
      } { c =>
        val program = code.substring(i + separator.length)
        Scalafmt.format(program, c).toEither match {
          case Right(formatted) =>
            implicit val sb = new StringBuilder
            mdPrePost(program.trim, formatted.trim)
            mdConfig("Config for this example:", config.trim)
            sb.toString()
          case Left(e: ParseException) =>
            reporter.error(pos, e.toString())
            "parse error"
          case Left(e) =>
            reporter.error(pos, e)
            "fail"
        }
      }
    }
  }

}

private object ScalafmtModifier {

  private val defaultConfig = ScalafmtConfig.default.copy(maxColumn = 40)
    .withDialect(scala.meta.dialects.Scala213, "scala213")

  private val defaultSbtConfig = defaultConfig.forSbt

  def mdBlock(lang: String, code: String)(implicit sb: StringBuilder): Unit = sb
    .append("```").append(lang).append("\n").append(code).append("\n```\n")

  /** Renders original and formatted side by side (the `.scalafmt-pair` styling
    * labels them "// Before" / "// After"). The blank lines around each fenced
    * block let CommonMark close the surrounding raw-HTML block and highlight
    * the code. Built without `stripMargin` on purpose: the embedded code often
    * has `stripMargin` continuations of its own, and `|` there must survive.
    */
  def mdPrePost(pre: String, post: String)(implicit sb: StringBuilder): Unit =
    div("scalafmt-pair") {
      div("before")(mdBlock("scala", pre))
      div("after")(mdBlock("scala", post))
    }

  def mdConfig(title: String, code: String)(implicit sb: StringBuilder): Unit =
    if (code.nonEmpty) html("details", "config", " open") {
      sb.append(s"<summary>$title</summary>\n\n")
      mdBlock("scala", code)
    }

  def html(tag: String, cls: String, attrs: String = "")(
      body: => Unit,
  )(implicit sb: StringBuilder): Unit = {
    sb.append("<").append(tag).append(" class=\"").append(cls).append("\"")
      .append(attrs).append(">\n\n")
    body
    sb.append("\n</").append(tag).append(">\n")
  }

  def div(cls: String)(body: => Unit)(implicit sb: StringBuilder): Unit =
    html("div", cls)(body)

}
