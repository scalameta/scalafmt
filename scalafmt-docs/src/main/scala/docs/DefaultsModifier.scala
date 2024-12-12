package docs

import org.scalafmt.config.ScalafmtConfig

import scala.meta.inputs.Input
import scala.meta.inputs.Position

import mdoc._
import metaconfig._

class DefaultsModifier extends StringModifier {
  override val name: String = "defaults"
  private val default = ConfEncoder[ScalafmtConfig]
    .writeObj(ScalafmtConfig.default)

  override def process(info: String, code: Input, reporter: Reporter): String =
    if (info == "all") {
      val result = Conf.printHocon(ScalafmtConfig.default)
      "```\n" + result + "\n```"
    } else {
      def getDefaultValue(param: String): String = {
        val path = param.split("\\.").toList
        val down = path.foldLeft(default.dynamic)(_ selectDynamic _)
        down.asConf.fold { e =>
          reporter.error(Position.Range(code, 0, 0), e.toString())
          "<fail>"
        }(_.toString())
      }

      val params = code.text.split("\\s+")
      if (params.length == 1) {
        val param = params(0)
        s"Default: `$param = ${getDefaultValue(param)}`\n"
      } else {
        val defaults = params.map(param => s"$param = ${getDefaultValue(param)}")
        ScalafmtModifier
          .mdConfigCodeBlock(defaults.mkString("# Defaults\n", "\n", ""))
      }
    }
}
