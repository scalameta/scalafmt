package docs

import metaconfig.Configured
import org.scalafmt.Scalafmt
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtRunner
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.parsers.ParseException
import mdoc.Reporter
import mdoc.StringModifier

class ScalafmtModifier extends StringModifier {
  override val name: String = "scalafmt"
  override def process(
      info: String,
      code: Input,
      reporter: Reporter
  ): String = {
    val runner =
      if (code.text.contains("package")) ScalafmtRunner.default
      else ScalafmtRunner.sbt
    val base = ScalafmtConfig.default.copy(runner = runner, maxColumn = 40)
    val i = code.text.indexOf("---")
    val pos = Position.Range(code, 0, 0)
    if (i == -1) {
      reporter.error(pos, "Missing ---")
      "fail"
    } else {
      val config = Input.Slice(code, 0, i)
      val program = Input.Slice(code, i + 3, code.chars.length)
      Config.fromHoconString(config.text, None, base) match {
        case Configured.Ok(c) =>
          val parsedConfig = c.copy(runner = runner)
          Scalafmt.format(program.text, parsedConfig).toEither match {
            case Right(formatted) =>
              val configText = config.text.trim
              val configBlock = s"<details class='config'><summary>Config for this example</summary><p>\n```yaml\n${configText}\n```\n</p></details>\n"
              val codeBlock = s"```scala\n${formatted.trim}\n```"
              val result = codeBlock + "\n" + configBlock
              result
            case Left(e: ParseException) =>
              reporter.error(pos, e.toString())
              "parse error"
            case Left(e) =>
              reporter.error(pos, e)
              "fail"
          }
        case Configured.NotOk(e) =>
          reporter.error(pos, e.toString())
          "fail"
      }
    }
  }
}
