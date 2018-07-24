import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtConfig.default40
import org.scalafmt.config.Config
import scala.meta.Dialect

package object website {

  private[this] def scalaCode(code: String): String =
    s"""|```scala
        |$code
        |```""".stripMargin

  /** Prints a formatted Scala code block one using the provided configuration,
    * which is added as a comment on top
    *
    * @param code the unformatted code
    * @param config the config as an HOCON string
    */
  def exampleBlock(code: String, config: String*): Unit = {
    val parsedConfig = Config
      .fromHoconString(config.mkString("\n"))
      .get
      .copy(maxColumn = 40, runner = ScalafmtRunner.sbt)
    val formattedCode = Scalafmt.format(code, parsedConfig).get
    println(
      scalaCode(
        s"""|${config.mkString("// ", "\n//", "")}
            |
            |${formattedCode}""".stripMargin
      ))
  }

  /** Prints two Scala code block next to each other, one with the original code,
    * the other one formatted using the provided configuration
    *
    * @param code the unformatted code
    * @param config the config to format the code (defaults to `default40`)
    */
  def compareExampleBlock(
      code: String,
      config: ScalafmtConfig = default40): Unit =
    println(
      s"""|<table width='100%' style='table-layout: fixed'>
          |<tbody>
          |<tr>
          |<td style='border: none'>
          |
          |${scalaCode(code)}
          |
          |</td>
          |<td style='border: none'>
          |
          |${scalaCode(Scalafmt.format(code, config).get)}
          |
          |</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin
    )

  def demo(code: String): Unit = {
    val formatted = Scalafmt.format(code, ScalafmtConfig.default40).get
    println(
      s"""
         |<div class='scalafmt-pair'>
         |  <div>
         |
         |${scalaCode(code)}
         |
         |  </div>
         |
         |  <div>
         |
         |${scalaCode(formatted)}
         |
         |  </div>
         |</div>
      """.stripMargin
    )
  }

  /** Prints the default value of a property
    *
    * @param selector a function to select the default from the config
    */
  def default[A](selector: ScalafmtConfig => A) = {
    val defaultValue = selector(ScalafmtConfig.default)
    println(s"Default: **$defaultValue**")
  }

}
