package docs

import metaconfig.Conf
import metaconfig.ConfEncoder
import metaconfig.Configured
import org.scalafmt.config.ScalafmtConfig
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import vork.Reporter
import vork.StringModifier

class DefaultsModifier extends StringModifier {
  override val name: String = "defaults"
  private val default =
    ConfEncoder[ScalafmtConfig].writeObj(ScalafmtConfig.default)

  override def process(
      info: String,
      code: Input,
      reporter: Reporter
  ): String = {
    if (info == "all") {
      val result =
        Conf.printHocon(ScalafmtConfig.default)
      "```\n" + result + "\n```"
    } else {
      val path = code.text.split("\\.").toList
      val down = path.foldLeft(default.dynamic)(_ selectDynamic _)
      down.asConf match {
        case Configured.Ok(value) =>
          "Default: `" + code.text.trim + " = " + value.toString + "`\n"
        case Configured.NotOk(e) =>
          reporter.error(Position.Range(code, 0, 0), e.toString())
          "fail"
      }
    }
  }
}
