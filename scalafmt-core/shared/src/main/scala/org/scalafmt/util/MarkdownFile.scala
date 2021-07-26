package org.scalafmt.util

import scala.meta.inputs.Position
import scala.meta.inputs.Input
import scala.collection.mutable

final case class MarkdownFile(input: Input, parts: List[MarkdownPart]) {
  private val appends = mutable.ListBuffer.empty[String]
  def appendText(text: String): Unit = {
    appends += text
  }
  def renderToString: String = {
    val out = new StringBuilder()
    parts.foreach(_.renderToString(out))
    appends.foreach(a => out.append(a).append("\n"))
    out.toString()
  }
}
object MarkdownFile {
  sealed abstract class State
  object State {
    case class CodeFence(start: Int, backticks: String, info: String)
        extends State
    case object Text extends State
  }
  class Parser(input: Input) {
    private val text = input.text
    private def newPos(start: Int, end: Int): Position = {
      Position.Range(input, start, end)
    }
    private def newText(start: Int, end: Int): Text = {
      val adaptedEnd = math.max(start, end)
      val part = Text(text.substring(start, adaptedEnd))
      part.pos = newPos(start, adaptedEnd)
      part
    }
    private def newCodeFence(
        state: State.CodeFence,
        backtickStart: Int,
        backtickEnd: Int
    ): CodeFence = {
      val open = newText(state.start, state.start + state.backticks.length())
      val info = newText(open.pos.end, open.pos.end + state.info.length())
      val adaptedBacktickStart = math.max(0, backtickStart - 1)
      val body = newText(info.pos.end, adaptedBacktickStart)
      val close = newText(adaptedBacktickStart, backtickEnd)
      val part = CodeFence(open, info, body, close)
      part.pos = newPos(state.start, backtickEnd)
      part
    }
    def acceptParts(): List[MarkdownPart] = {
      var state: State = State.Text
      val parts = mutable.ListBuffer.empty[MarkdownPart]
      var curr = 0
      text.linesWithSeparators.foreach { line =>
        val end = curr + line.length()
        state match {
          case State.Text =>
            if (line.startsWith("```")) {
              val backticks = line.takeWhile(_ == '`')
              val info = line.substring(backticks.length())
              state = State.CodeFence(curr, backticks, info)
            } else {
              parts += newText(curr, end)
            }
          case s: State.CodeFence =>
            if (
              line.startsWith(s.backticks) &&
              line.forall(ch => ch == '`' || ch.isWhitespace)
            ) {
              parts += newCodeFence(s, curr, end)
              state = State.Text
            }
        }
        curr += line.length()
      }
      state match {
        case s: State.CodeFence =>
          parts += newCodeFence(s, text.length(), text.length())
        case _ =>
      }
      parts.toList
    }
  }
  def parse(input: Input): MarkdownFile = {
    val parser = new Parser(input)
    val parts = parser.acceptParts()
    MarkdownFile(input, parts)
  }
}

sealed abstract class MarkdownPart {
  var pos: Position = Position.None
  final def renderToString(out: StringBuilder): Unit =
    this match {
      case Text(value) =>
        out.append(value)
      case fence: CodeFence =>
        fence.newPart match {
          case Some(newPart) =>
            out.append(newPart)
          case None =>
            fence.openBackticks.renderToString(out)
            fence.newInfo match {
              case None =>
                fence.info.renderToString(out)
              case Some(newInfo) =>
                out.append(newInfo)
                if (!newInfo.endsWith("\n")) {
                  out.append("\n")
                }
            }
            fence.newBody match {
              case None =>
                fence.body.renderToString(out)
              case Some(newBody) =>
                out.append(newBody)
            }
            fence.closeBackticks.renderToString(out)
        }
    }
}
final case class Text(value: String) extends MarkdownPart
final case class CodeFence(
    openBackticks: Text,
    info: Text,
    body: Text,
    closeBackticks: Text
) extends MarkdownPart {
  var newPart = Option.empty[String]
  var newInfo = Option.empty[String]
  var newBody = Option.empty[String]
}
