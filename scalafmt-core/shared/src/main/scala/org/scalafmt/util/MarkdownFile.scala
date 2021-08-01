package org.scalafmt.util

import scala.meta.inputs.Input
import scala.collection.mutable

final case class MarkdownFile(parts: List[MarkdownPart]) {
  def renderToString: String = {
    val out = new StringBuilder()
    parts.foreach(_.renderToString(out))
    out.result()
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

    private def substringWithAdaptedEnd(start: Int, end: Int): String = {
      val adaptedEnd = math.max(start, end)
      text.substring(start, adaptedEnd)
    }

    private def newCodeFence(
        state: State.CodeFence,
        backtickStart: Int,
        backtickEnd: Int
    ): MarkdownPart.CodeFence = {
      val open = substringWithAdaptedEnd(
        state.start,
        state.start + state.backticks.length()
      )
      val lastIndexOfOpeningBackTicks = state.start + state.backticks.length()
      val info = substringWithAdaptedEnd(
        lastIndexOfOpeningBackTicks,
        lastIndexOfOpeningBackTicks + state.info.length()
      )
      val adaptedBacktickStart = math.max(0, backtickStart - 1)
      val body = substringWithAdaptedEnd(
        lastIndexOfOpeningBackTicks + info.length(),
        adaptedBacktickStart
      )
      val close = substringWithAdaptedEnd(adaptedBacktickStart, backtickEnd)
      MarkdownPart.CodeFence(open, info, body, close)
    }
    def acceptParts(): List[MarkdownPart] = {
      var state: State = State.Text
      val parts = mutable.ListBuffer.empty[MarkdownPart]
      var curr = 0
      text.linesWithSeparators
        .foreach { line =>
          val end = curr + line.length()
          state match {
            case State.Text =>
              if (line.startsWith("```")) {
                val backticks = line.takeWhile(_ == '`')
                val info = line.substring(backticks.length())
                state = State.CodeFence(curr, backticks, info)
              } else {
                parts += MarkdownPart.Text(substringWithAdaptedEnd(curr, end))
              }
            case s: State.CodeFence =>
              if (
                line.startsWith(s.backticks) &&
                line.forall(ch => ch == '`' || ch.isSpaceChar)
              ) {
                parts += newCodeFence(s, curr, end)
                state = State.Text
              }
          }
          curr = end
        }
      state match {
        case s: State.CodeFence =>
          parts += newCodeFence(s, text.length(), text.length())
        case _ =>
      }
      parts.toList
    }
  }

  def parse(input: Input): MarkdownFile =
    MarkdownFile(new Parser(input).acceptParts())

}

sealed abstract class MarkdownPart {
  final def renderToString(out: StringBuilder): Unit =
    this match {
      case MarkdownPart.Text(value) =>
        out.append(value)
      case fence: MarkdownPart.CodeFence =>
        out.append(fence.openBackticks)
        out.append(fence.info)
        fence.newBody match {
          case None =>
            out.append(fence.body)
          case Some(newBody) =>
            out.append(newBody)
        }
        out.append(fence.closeBackticks)
    }
}
object MarkdownPart {
  final case class Text(value: String) extends MarkdownPart
  final case class CodeFence(
      openBackticks: String,
      info: String,
      body: String,
      closeBackticks: String
  ) extends MarkdownPart {
    var newBody = Option.empty[String]
  }

}
