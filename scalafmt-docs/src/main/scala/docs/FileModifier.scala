package docs

import java.nio.charset.StandardCharsets
import mdoc.Reporter
import mdoc.StringModifier
import mdoc.internal.pos.PositionSyntax._
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath

class FileModifier extends StringModifier {
  val name = "file"
  override def process(
      info: String,
      code: Input,
      reporter: Reporter
  ): String = {
    val file = AbsolutePath(info)
    if (file.isFile) {
      val text = FileIO.slurp(file, StandardCharsets.UTF_8).trim
      val language = PathIO.extension(file.toNIO) match {
        case "scala" => "scala"
        case "md" => "md"
        case _ => "text"
      }
      s"""
File: [${file.toNIO.getFileName}](https://github.com/scalameta/mdoc/blob/master/$info)
`````$language
$text
`````
"""
    } else {
      val pos =
        Position.Range(code, 0 - info.length - 1, 0 - 1).toUnslicedPosition
      reporter.error(pos, s"no such file: $file")
      ""
    }
  }

}
