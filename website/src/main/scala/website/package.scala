import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtConfig.default40
import org.scalafmt.config.Config

package object website {
  def exampleBlock(code: String, config: String): Unit =
    exampleBlock(code, Config.fromHoconString(config).get.copy(maxColumn = 40))

  def exampleBlock(code: String, config: ScalafmtConfig = default40): Unit =
    println(
      s"""|<table width='100%'>
          |<tbody>
          |<tr>
          |<td style='border: none'>
          |
          |```scala
          |$code
          |```
          |
          |</td>
          |<td style='border: none'>
          |
          |```scala
          |${Scalafmt.format(code, config).get}
          |```
          |
          |</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin
    )

}
