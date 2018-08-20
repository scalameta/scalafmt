package docs

import metaconfig.Conf
import metaconfig.ConfEncoder
import metaconfig.Configured
import org.scalafmt.config.ScalafmtConfig
import scala.meta.inputs.Input
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
    val head :: rest = code.text.split("\\.").toList
    default.get[Conf](head, rest: _*) match {
      case Configured.Ok(value) =>
        "Default: `" + code.text.trim + " = " + value.toString + "`\n"
      case Configured.NotOk(e) =>
        reporter.error(e.toString())
        "fail"
    }
  }
}
