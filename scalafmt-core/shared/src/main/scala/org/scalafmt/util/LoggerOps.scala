package org.scalafmt.util

import org.scalafmt.internal._

import org.scalameta.FileLine
import scala.meta.Tree
import scala.meta.inputs.InputRange
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Tokens
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

import sourcecode.Text

/** Debugging utility.
  */
object LoggerOps {
  private val loggerLike = new DynamicVariable[LoggerLike](Logger)
  def logger = loggerLike.value
  def logger(flag: Boolean): Unit =
    loggerLike.value = if (flag) Logger else NoLogger

  // TODO(olafur) parameterize
  def name2style[T](styles: Text[T]*): Map[String, T] = styles
    .map(x => x.source -> x.value).toMap

  def log(s: State, indent: String = "", nlIndices: Boolean = true): String = {
    val delim = s"\n$indent  "
    val policies = s.policy.policies match {
      case Nil => ""
      case p :: Nil => s";${delim}P($p)"
      case pp => pp.map(_.toString).mkString(s";${delim}P[", s",$delim  ", s"]")
    }
    @tailrec
    def getNlIndices(x: State, res: List[String]): List[String] =
      if (x.depth == 0) res
      else getNlIndices(
        x.prev,
        if (x.split.isNL) s"${x.cost}@${x.depth}" :: res else res,
      )
    val nls =
      if (nlIndices) getNlIndices(s, Nil).mkString(s"${delim}nl=[", ",", "]")
      else ""
    s"d=${s.depth} w=${s.cost}[${s.appliedPenalty}] i=${s.indentation} col=${s
        .column} #nl=${s.lineId}$policies;${delim}s=${log(s.split)}$nls"
  }
  def log(split: Split): String = split.toString

  def log(formatToken: FT): String =
    s"""|${log(formatToken.left)}
        |${log(formatToken.between: _*)}
        |${log(formatToken.right)}""".stripMargin

  def log2(formatToken: FT): String = formatToken.toString
  def log2(formatToken: Option[FT]): String = formatToken.fold("")(log2)

  def ftokWithoutPos(ft: FT): String = {
    val lt = ft.meta.left.text
    val rt = ft.meta.right.text
    val ls = tokWithoutPos(ft.left)
    val rs = tokWithoutPos(ft.right)
    val lo = treeNameWithParent(ft.leftOwner)
    val ro = treeNameWithParent(ft.rightOwner)
    s"[${ft.idx}] $lt $rt >>> $ls | $rs >>> $lo | $ro"
  }

  def escape(raw: String): String = raw

  def log(tokens: T*): String = tokens.map(log).mkString("\n")

  def cleanup(token: T): String = token match {
    case _: T.Literal | _: T.Interpolation.Part => escape(token.syntax)
        .stripPrefix("\"").stripSuffix("\"")
    case _ => token.syntax.replace("\n", "")
  }

  def log(tokens: Tokens): String = tokens.map(log).mkString("\n")

  def log(token: T): String = logTok(token)
  def logTok(token: T): String = f"[${token.structure}%-40s"
  def logTok(token: Option[T]): String = token.fold("")(log)

  def tokWithoutPos(token: T): String = {
    val desc = token.structure
    val posidx = desc.lastIndexOf('[')
    val len = if (posidx > 0) posidx - 1 else desc.length
    val txtidx = desc.indexOf('(')
    if (txtidx < 0 || len <= txtidx + 52) desc.substring(0, len)
    else desc.substring(0, txtidx + 50) + "...)"
  }

  def log(range: InputRange): String = s"[${range.start}..${range.end})"

  def position(t: Tree): String = log(t.pos)

  def treeName(t: Tree): String = {
    val typeName = t.getClass.getName.stripPrefix("scala.meta.")
    val parts = typeName.split('$')
    parts.length match {
      case 0 => typeName
      case 1 => parts(0)
      case _ => s"${parts(0)}.${parts(1)}"
    }
  }

  def treeName(t: Option[Tree]): String = t.fold("")(treeName)

  def treeNameWithParent(t: Tree): String =
    s"${treeName(t)} [${treeName(t.parent)}]"

  def treeInfo(t: Tree): String =
    s"${treeName(t)} ${position(t)} [${treeName(t.parent)}]"

  def log(t: Tree): String = logTree(t, noStructure = false)
  def logTree(
      t: Tree,
      tokensOnly: Boolean = false,
      noStructure: Boolean = true,
  ): String = {
    val tokens = s"TOKENS: ${t.tokens.map(x => reveal(x.text)).mkString(",")}"
    if (tokensOnly) tokens
    else s"""|TYPE: ${treeInfo(t)}
             |SOURCE: $t
             |STRUCTURE: ${if (noStructure) "" else t.show[Structure]}
             |$tokens
             |""".stripMargin
  }

  def log(t: Option[Tree]): String = logTreeOpt(t, noStructure = false)
  def logTreeOpt(
      t: Option[Tree],
      tokensOnly: Boolean = false,
      noStructure: Boolean = true,
  ): String = t
    .fold("")(logTree(_, tokensOnly = tokensOnly, noStructure = noStructure))

  def reveal(s: String): String = s.map {
    case '\n' => '¶'
    case ' ' => '∙'
    case ch => ch
  }

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  sealed trait LoggerLike {
    def println(x: Any): Unit
    def debug(x: Any)(implicit fileLine: FileLine): Unit
    def elem(values: sourcecode.Text[Any]*)(implicit fileLine: FileLine): Unit
  }
  object Logger extends LoggerLike {
    def println(x: Any): Unit = Console.out.println(x)
    def debug(x: Any)(implicit fileLine: FileLine): Unit = org.scalameta.logger
      .debug(x)
    def elem(values: sourcecode.Text[Any]*)(implicit fileLine: FileLine): Unit =
      org.scalameta.logger.elem(values: _*)
  }
  object NoLogger extends LoggerLike {
    def println(x: Any): Unit = {}
    def debug(x: Any)(implicit fileLine: FileLine): Unit = {}
    def elem(values: sourcecode.Text[Any]*)(implicit fileLine: FileLine): Unit = {}
  }

  def logDebugRoutes(
      routes: IndexedSeq[Seq[Split]],
      ftoks: FormatTokens,
  ): Unit = if (null ne routes) {
    var tokidx = 0
    val toks = ftoks.arr
    while (tokidx < toks.length) {
      logger.debug(s"FT: ${ftokWithoutPos(toks(tokidx))}")
      routes(tokidx).foreach(s => logger.debug(s"> S: ${log(s)}"))
      tokidx += 1
    }
  }

  def logDebugStateStack(finalState: State, ftoks: FormatTokens): Unit =
    if ((null ne finalState) && (finalState ne State.start)) {
      val toks = ftoks.arr
      val stack = new mutable.ListBuffer[String]
      val posWidth = s"%${1 + math.log10(toks.last.left.end).toInt}d"
      @tailrec
      def iter(state: State): Unit = if (state.prev ne State.start) {
        val prev = state.prev
        val idx = prev.depth
        val tok = toks(idx).left
        val clean = "%-15s".format(cleanup(tok).slice(0, 15))
        stack.prepend(s"[$idx] ${posWidth.format(tok.end)}: $clean" + s" ${state
            .split} ${prev.indentation} ${prev.column} [${state.cost}]")
        iter(prev)
      }
      iter(finalState)
      stack.foreach(logger.debug)
      logger.debug(s"Total cost: ${finalState.cost}")
    }

}
