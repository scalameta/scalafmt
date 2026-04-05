package org.scalafmt.rewrite

import org.scalafmt.config._

object FileHeaderOps {

  private def currentYear: Int = java.time.Year.now().getValue

  def apply(content: String, style: ScalafmtConfig, range: Set[Range]): String = {
    if (range.nonEmpty) return content

    val cfg = style.fileHeader
    val headerOpt = resolveHeader(cfg, style)
    if (headerOpt.isEmpty) return content

    val expectedHeader = headerOpt.get
    val parts = splitFile(content)

    if (parts.formatOff) return content

    // Strip mode: active but empty content - remove existing header
    if (expectedHeader.isEmpty) {
      if (parts.header.isEmpty) return content
      val bodyTrimmed = parts.body.dropWhile(_.isWhitespace)
      return s"${parts.preamble}${if (bodyTrimmed.nonEmpty) "\n" else ""}$bodyTrimmed"
    }

    // Idempotent short-circuit
    if (parts.header == expectedHeader) return content

    val preambleSep = if (parts.preamble.nonEmpty) "\n" else ""
    val bodySep = parts.body match {
      case b if b.isEmpty => "\n"
      case _ => if (cfg.blankLineAfter) "\n\n" else "\n"
    }
    val bodyContent = parts.body.dropWhile(_.isWhitespace)

    s"${parts.preamble}$preambleSep$expectedHeader$bodySep$bodyContent"
  }

  // -- Content resolution --

  private def resolveHeader(
      cfg: FileHeader,
      style: ScalafmtConfig,
  ): Option[String] =
    if (!cfg.isActive) None
    else cfg.raw.map(_.stripMargin.trim).orElse {
      resolveInnerContent(cfg).map { c =>
        if (c.isEmpty) ""
        else wrapInComment(c, cfg, style)
      }
    }

  private def resolveInnerContent(cfg: FileHeader): Option[String] =
    cfg.text.map(_.stripMargin.trim)
      .orElse(cfg.license.map(generateFromLicense(_, cfg)))

  private def generateFromLicense(
      license: License,
      cfg: FileHeader,
  ): String = {
    val year = cfg.year.getOrElse(currentYear)
    val yearStr = cfg.since match {
      case Some(s) if s < year => s"$s-$year"
      case Some(s)             => s.toString
      case None if cfg.year.isDefined => year.toString
      case None                => ""
    }
    val copyrightLine = cfg.copyrightHolder.map { holder =>
      val yr = if (yearStr.nonEmpty) s"$yearStr " else ""
      s"Copyright $yr$holder"
    }
    cfg.licenseStyle match {
      case FileHeader.LicenseStyle.spdx =>
        (copyrightLine.toSeq :+ s"SPDX-License-Identifier: $license")
          .mkString("\n")
      case FileHeader.LicenseStyle.detailed =>
        License.detailed(license, copyrightLine)
    }
  }

  // -- Comment wrapping --

  private def wrapInComment(
      content: String,
      cfg: FileHeader,
      style: ScalafmtConfig,
  ): String = {
    val comment = cfg.comment
    val blankFirst = comment.blankFirstLine
      .orElse(style.docstrings.blankFirstLine)
      .forall(_ ne Docstrings.BlankFirstLine.fold)
    val blankLast = comment.blankLastLine
    cfg.style match {
      case FileHeader.Style.block =>
        val blockStyle = comment.style.getOrElse(style.docstrings.style)
        wrapBlock(content, blockStyle, blankFirst, blankLast)
      case FileHeader.Style.line => wrapLine(content)
      case FileHeader.Style.framed =>
        wrapFramed(
          content,
          comment.width.getOrElse(style.maxColumn),
          blankFirst,
          blankLast,
        )
    }
  }

  private def wrapBlock(
      content: String,
      blockStyle: Docstrings.Style,
      blankFirst: Boolean,
      blankLast: Boolean,
  ): String = {
    val (prefix, closer) = blockStyle match {
      case Docstrings.Asterisk => ("*", "*/")
      case _                   => (" *", " */")
    }
    val last = if (blankLast) s"\n$prefix" else ""
    val contentLines = content.linesIterator.toSeq
    val prefixed = contentLines
      .map(l => if (l.isEmpty) prefix else s"$prefix $l")
    if (blankFirst)
      // First line blank: /*
      //                    * content
      s"/*\n${prefixed.mkString("\n")}$last\n$closer"
    else {
      // First line has content: /* content
      //                          * more
      val rest = prefixed.tail
      val mid = if (rest.isEmpty) "" else s"\n${rest.mkString("\n")}"
      s"/* ${contentLines.head}$mid$last\n$closer"
    }
  }

  private def wrapLine(content: String): String =
    content.linesIterator
      .map(l => if (l.isEmpty) "//" else s"// $l")
      .mkString("\n")

  private def wrapFramed(
      content: String,
      width: Int,
      blankFirst: Boolean,
      blankLast: Boolean,
  ): String = {
    val innerWidth = width - 5
    val contentLines = content.linesIterator.toSeq
    val maxLen = if (contentLines.isEmpty) 0 else contentLines.map(_.length).max
    val leftPad = " " * ((innerWidth - maxLen) / 2).max(0)
    val blankLine = " *" + " " * (width - 3) + "*"

    val top = "/" + "*" * (width - 1)
    val bottom = " " + "*" * (width - 2) + "/"
    val lines = contentLines.map { l =>
      if (l.isEmpty) blankLine
      else {
        val padded = (leftPad + l).take(innerWidth).padTo(innerWidth, ' ')
        s" * $padded *"
      }
    }
    val first = if (blankFirst) Seq(blankLine) else Seq.empty
    val last = if (blankLast) Seq(blankLine) else Seq.empty
    (Seq(top) ++ first ++ lines ++ last ++ Seq(bottom)).mkString("\n")
  }

  // -- File decomposition --

  private case class FileParts(
      preamble: String,
      header: String,
      body: String,
      formatOff: Boolean,
  )

  // Reuses the canonical format-off strings from ScalafmtConfig (lines 550-556)
  private def isFormatOff(commentContent: String): Boolean =
    ScalafmtConfig.defaultFormatOff.contains(commentContent.trim.toLowerCase)

  // Matches the scalafmt directive pattern from StyleMap (line 16)
  private val scalafmtDirectivePattern = "\\s*scalafmt: ".r

  private def isConfigOverride(commentContent: String): Boolean =
    scalafmtDirectivePattern.findPrefixOf(commentContent).isDefined

  private def splitFile(content: String): FileParts = {
    val len = content.length
    var pos = 0

    // BOM detection
    if (len > 0 && content.charAt(0) == '\uFEFF') pos = 1

    // Shebang detection
    if (pos < len && content.startsWith("#!", pos)) {
      val nl = content.indexOf('\n', pos)
      pos = if (nl < 0) len else nl + 1
    }

    val preambleEnd = pos

    // Skip whitespace before first comment
    while (pos < len && content.charAt(pos).isWhitespace) pos += 1

    // Scan leading comments
    var formatOff = false
    var headerStart = -1
    var headerEnd = -1
    var scanning = true

    while (scanning && pos < len) {
      if (content.startsWith("//", pos)) {
        // Single line comment - read to end of line
        val lineEnd = content.indexOf('\n', pos)
        val end = if (lineEnd < 0) len else lineEnd
        val commentText = content.substring(pos + 2, end)

        if (isFormatOff(commentText)) {
          formatOff = true
          scanning = false
        } else if (isConfigOverride(commentText)) {
          // Skip this line, continue scanning
          pos = if (lineEnd < 0) len else lineEnd + 1
          while (pos < len && content.charAt(pos).isWhitespace) pos += 1
        } else {
          // Found header - consume contiguous // lines
          headerStart = pos
          headerEnd = findLineCommentsEnd(content, pos)
          scanning = false
        }
      } else if (content.startsWith("/*", pos)) {
        val blockEnd = findBlockCommentEnd(content, pos)
        if (blockEnd < 0) {
          // Unterminated block comment - treat as no header
          scanning = false
        } else {
          val commentText = content.substring(pos + 2, blockEnd - 2)
          if (isFormatOff(commentText)) {
            formatOff = true
            scanning = false
          } else if (isConfigOverride(commentText)) {
            pos = blockEnd
            while (pos < len && content.charAt(pos).isWhitespace) pos += 1
          } else {
            headerStart = pos
            headerEnd = blockEnd
            scanning = false
          }
        }
      } else {
        // Not a comment - no header found
        scanning = false
      }
    }

    if (formatOff) return FileParts(
      content.substring(0, preambleEnd),
      "",
      content.substring(preambleEnd),
      formatOff = true,
    )

    if (headerStart >= 0) {
      // Preamble is only BOM/shebang. Directives between preamble and header
      // go into the body so the header is always the first comment in output.
      val preamble = content.substring(0, preambleEnd)
      val header = content.substring(headerStart, headerEnd)
      val beforeHeader = content.substring(preambleEnd, headerStart)
      val afterHeader = content.substring(headerEnd)
      FileParts(preamble, header, beforeHeader + afterHeader, formatOff = false)
    } else {
      FileParts(
        content.substring(0, preambleEnd),
        "",
        content.substring(preambleEnd),
        formatOff = false,
      )
    }
  }

  /** Finds end position of block comment starting at `pos`, or -1. */
  private def findBlockCommentEnd(content: String, pos: Int): Int = {
    val idx = content.indexOf("*/", pos + 2)
    if (idx < 0) -1 else idx + 2
  }

  /** Find the end of contiguous `//` comment lines starting at `pos`. Returns
    * the position of the newline after the last `//` line (or end of string),
    * so that the header text does not include the trailing newline.
    */
  private def findLineCommentsEnd(content: String, pos: Int): Int = {
    val len = content.length
    var p = pos
    var lastEnd = pos
    while (p < len && content.startsWith("//", p)) {
      val nl = content.indexOf('\n', p)
      if (nl < 0) { lastEnd = len; p = len }
      else { lastEnd = nl; p = nl + 1 }
    }
    lastEnd
  }

}
