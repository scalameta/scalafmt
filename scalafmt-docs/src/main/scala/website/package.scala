import java.io.PrintStream
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.Config

package object website {
  def replaceMargin(s: String): String = {
    val buf = new StringBuilder

    for (line <- s.linesIterator) {
      val len = line.length
      var index = 0
      while (index < len && line.charAt(index) <= ' ') {
        index += 1
        buf.append(' ')
      }
      if (index < len) {
        line.charAt(index) match {
          case '#' => buf.append('|')
          case ch => buf.append(ch)
        }
        index += 1
      }
      if (index < len) {
        buf.append(line.substring(index))
      }
      buf.append('\n')
    }
    buf.toString
  }

  def plaintext(code: String): String =
    "```\n" + code + "\n```"

  private[this] def scalaCode(code: String): String =
    "```scala\n" + code + "\n```"

  def preProcess(code: String): String =
    replaceMargin(code.trim).replace("'''", "\"\"\"")

  val logger: PrintStream = {
    val out = Files.newOutputStream(
      Paths.get("target", "website.log"),
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )
    new PrintStream(out)
  }

  /** Prints a formatted Scala code block one using the provided configuration,
    * which is added as a comment on top
    *
    * @param code
    *   the unformatted code * @param config the config as an HOCON string
    */
  def exampleBlock(code: String, config: String*): Unit = {
    example(code, config, ScalafmtRunner.sbt)
  }
  def exampleSource(code: String, config: String*): Unit = {
    example(code, config, ScalafmtRunner.default)
  }
  def example(
      code: String,
      config: Seq[String],
      runner: ScalafmtRunner
  ): Unit = {
    val processedCode = preProcess(code)
    val parsedConfig1 =
      Config.fromHoconString(config.mkString("\n")).get.copy(runner = runner)
    val isCustomMaxColumn = config.exists(_.contains("maxColumn"))
    val parsedConfig =
      if (isCustomMaxColumn) parsedConfig1
      else parsedConfig1.copy(maxColumn = 40)
    if (code.contains("validatedInstances")) {
      logger.println("=======")
      logger.println(
        parsedConfig.newlines.sometimesBeforeColonInMethodReturnType
      )
      logger.println(parsedConfig.maxColumn)
      logger.println()
      logger.println(config.mkString("\n"))
    }
    val formattedCode = Scalafmt.format(processedCode, parsedConfig).get
    val result = new StringBuilder()
      .append(config.mkString("// ", "\n// ", ""))
      .append("\n\n")
      .append(formattedCode)
      .toString()

    println(scalaCode(result))
  }

  /** Prints two Scala code block next to each other, one with the original
    * code, the other one formatted using the provided configuration
    *
    * @param code
    *   the unformatted code
    * @param config
    *   the config to format the code (defaults to `default40`)
    */
  def formatExample(code: String, config: String*): Unit = {
    val parsedConfig = Config
      .fromHoconString(config.mkString("\n"))
      .get
      .copy(maxColumn = 40, runner = ScalafmtRunner.sbt)
    val processedCode = preProcess(code)
    val formatted = Scalafmt.format(processedCode, parsedConfig).get
    val configString =
      if (config.isEmpty) ""
      else {
        // TODO: make this pretty
        s"""
<div class="scalafmt-configuration">
<pre><code>${config.mkString("\n").trim}</pre></code>
</div>
""".trim
      }
    println(
      s"""
<div class='scalafmt-pair'>
  <div class='before'>

${scalaCode(processedCode)}

  </div>

  <div class='after'>

${scalaCode(formatted)}

  </div>
</div>
$configString
"""
    )
  }

  /** Prints the default value of a property
    *
    * @param selector
    *   a function to select the default from the config
    */
  def default[A](selector: ScalafmtConfig => A) = {
    val defaultValue = selector(ScalafmtConfig.default)
    println(s"Default: **$defaultValue**")
  }

}
