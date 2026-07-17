package docs

import org.scalafmt.config.ScalafmtConfig

import scala.meta.inputs.{Input, Position}

import mdoc._
import metaconfig._

class DefaultsModifier extends StringModifier {
  override val name: String = "defaults"
  private val default = ConfEncoder[ScalafmtConfig]
    .writeObj(ScalafmtConfig.default)

  override def process(
      info: String,
      code: Input,
      reporter: Reporter,
  ): String = {
    val result =
      if (info == "all") Conf.printHocon(ScalafmtConfig.default)
      else processCode(code, reporter)
    "```properties\n" + result + "\n```"
  }

  private def processCode(code: Input, reporter: Reporter): String = {
    def getDefaultValue(param: String): String = {
      val path = param.split("\\.").toList
      val down = path.foldLeft(default.dynamic)(_ selectDynamic _)
      down.asConf.fold { e =>
        reporter.error(Position.Range(code, 0, 0), e.toString())
        "<fail>"
      }(_.toString())
    }

    code.text.split("\\s+").map(p => s"$p = ${getDefaultValue(p)}")
      .mkString("# Defaults\n", "\n", "")
  }

}
