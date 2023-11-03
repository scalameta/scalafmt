package docs

import org.scalafmt.Scalafmt
import org.scalafmt.config.NamedDialect
import org.scalafmt.config.ScalafmtConfig
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.parsers.ParseException
import mdoc.Reporter
import mdoc.StringModifier

class ScalafmtModifier extends StringModifier {

  import ScalafmtModifier._

  private final val separator = "---\n"

  override val name: String = "scalafmt"
  override def process(
      info: String,
      code: Input,
      reporter: Reporter
  ): String = {
    val base =
      if (code.text.contains("package")) defaultConfig else defaultSbtConfig
    val i = code.text.indexOf(separator)
    val pos = Position.Range(code, 0, 0)
    if (i == -1) {
      reporter.error(pos, "Missing ---")
      "fail"
    } else {
      val config = Input.Slice(code, 0, i)
      val program = Input.Slice(code, i + separator.length, code.chars.length)
      val configured = ScalafmtConfig.fromHoconString(config.text, base)
      configured.fold { e =>
        reporter.error(pos, e.toString())
        "fail"
      } { c =>
        Scalafmt.format(program.text, c).toEither match {
          case Right(formatted) =>
            val configText = config.text.trim
            val configBlock =
              if (configText == "") ""
              else mdConfigSection("Config for this example:", configText)

            val formattedCodeBlock =
              mdScalaCodeBlock("formatted", formatted.trim)
            val originalCodeBlock =
              mdScalaCodeBlock("original", program.text.trim)
            List(
              formattedCodeBlock,
              originalCodeBlock,
              configBlock
            ).mkString("\n")
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

object ScalafmtModifier {

  private val defaultConfig = ScalafmtConfig.default
    .copy(maxColumn = 40)
    .withDialect(NamedDialect.scala213, "scala213")

  private val defaultSbtConfig = defaultConfig.forSbt

  def mdCodeBlock(language: String, content: String): String =
    s"```$language\n$content\n```"

  def mdScalaCodeBlock(style: String, content: String): String =
    mdCodeBlock(s"scala $style", content)

  def mdConfigCodeBlock(content: String): String =
    mdScalaCodeBlock("config", content)

  def mdConfigSection(title: String, code: String): String = {
    val content = mdConfigCodeBlock(code)
    s"<details class='config' open><summary>$title</summary><p>\n$content\n</p></details>\n"
  }

}
