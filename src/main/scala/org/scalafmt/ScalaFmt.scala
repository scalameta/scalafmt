package org.scalafmt

import scala.collection.mutable
import scala.meta. _
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Term.Block
import scala.meta.internal.ast.Term.Interpolate
import scala.meta.parsers.common.Parse
import scala.meta.tokens.Token
import scala.meta.tokens.Token.`(`
import scala.meta.tokens.Token.`)`
import scala.meta.tokens.Token.`[`
import scala.meta.tokens.Token.`]`
import scala.meta.tokens.Token.`{`
import scala.meta.tokens.Token.`}`

class ScalaFmt(val style: ScalaStyle) extends ScalaFmtLogger {
  val MaxVisits = 10000

  /**
    * Formats a Scala compilation unit.
    *
    * For example a source file:
    *
    * $ cat MyCode.scala
    * package foo
    *
    * import a.b
    *
    * object c {
    * def x = 2
    * ...
    * }
    *
    */
  def formatSource(code: String): String = format[Source](code)

  /**
    * Formats a single Scala statement.
    *
    * For example a function call:
    *
    * function(function(a, b), function(c, d))
    */
  def formatStatement(code: String): String = format[Stat](code)

  /**
    * Formats any kind of [[scala.meta.Tree]].
    *
    * Most likely, [[formatSource()]] or [[formatStatement()]] is what you need.
    *
    * @param code The source code to format.
    * @param ev The implicit evidence that the source code can be parsed.
    * @tparam T The type of the source code, refer to [[scala.meta.parsers.Api]]
    *           for available types.
    * @return The source code formatted.
    */
  def format [T <: Tree](code: String,
          rangeOpt: Option[Range] = None) (implicit ev: Parse[T]): String = {
    val range = rangeOpt.getOrElse(Range(0, Int.MaxValue))
    try {
      val source = code.parse[T]
      formatTree(source, range)
    }
    catch {
      // Skip invalid code.
      case e: ParseException =>
        logger.warn(s"Unable to parse code: ${e.getMessage}")
        code
    }
  }

  private def formatTree(tree: Tree, range: Range): String = {
    val toks = FormatToken.formatTokens(tree.tokens)
    val owners = getOwners(tree)
    val statementStarts = getStatementStarts(tree)
    val memo = mutable.Map.empty[( Int, Tree), State]
    var explored = 0
    var deepestYet = State.start
    val best = mutable.Map.empty[Token, State]
    val parens = matchingParens(tree.tokens)
    val formatter =
      new Formatter(style, tree, toks, parens, statementStarts, owners)
    // Keeps track of optimalAt optimization.
    // TODO(olafur) inefficient to key by Vector[Split]? The split prefix is what matters.
    val optimal = mutable.Map.empty[( Int, FormatToken), Split]

    def mkString(splits: Vector[Split]): String = {
      val output = State.reconstructPath(toks, splits, style)
      val sb = new StringBuilder()
      output.foreach {
        case (tok, whitespace) =>
          sb.append(tok.left.code)
          sb.append(whitespace)
      }
      if (sb.last != '\n') // Always trailing newline.
        sb.append('\n')
      sb.toString()
    }

    /**
      * Same as shortest path except caches results.
      */
    def shortestPathMemo(owner: Tree, start: State, prev: State): State = {
      val col = start.indentation
      val i = Math.max(0, prev.splits.length - 1)
      val key = col -> owner
      memo.getOrElseUpdate(key, {
            val result = shortestPath(owner, start)
            val output = mkString(result.splits)
            logger.trace(
                s"""${result.cost} ${result.column} ${prev.column} ${prev.indentation} ${start.column} ${toks(i)}
             |${header("splits")}
             |${start.splits}
             |${header("stripped output")}
             |${output.stripPrefix(mkString(start.splits))}
             |${header("prev.splits")}
             |${mkString(prev.splits).length}
             |${mkString(prev.splits)}
             |${reveal(mkString(prev.splits))}
             |${header("output")}
             |${mkString(result.splits)}
             |""".stripMargin)
            result
          })
    }

    // TODO(olafur) refactor shortestPath

    /**
      * Runs Dijstra's shortest path algorithm to find lowest penalty split.
      */
    def shortestPath(owner: Tree, start: State): State = {
      Debug.visit(owner)
      logger.trace(
          s"""${start.indentation} ${start.splits.takeRight(3)}
           |${log(owner)}
           |FORMAT:
           |${State.reconstructPath(toks,start.splits,style)}""".stripMargin)

      /**
        * Returns true if it's OK to skip over state.
        */
      def pruneOK(state: State): Boolean = {
        val splitToken = toks(state.splits.length)
        // TODO(olafur) inefficient
        var curr = State.start
        val hasOptimal = state.splits.zipWithIndex.forall {
          case (split, i) =>
            val tok = toks(i)
            val result = optimal.get(curr.column -> tok)
              .forall(_.sameLine(split))
            if (!result)
              logger.trace(
                  s"""
                   |${header(s"$split eliminated $state at ${toks(i)}, $tok")}
                   |${mkString(state.splits)}
                   |${optimal.toVector.mkString("\n")}""".stripMargin)
            curr = curr.next(style, split, tok)
            result
        }
        val hasBest = best.get(splitToken.left).exists(_.alwaysBetter(state))
        !hasOptimal|| hasBest
      }

      def updateOptimal(tok: FormatToken, curr: State): Unit = {
        // TODO(olafur) inefficient
        var state = State.start
        curr.splits.zipWithIndex.foreach {
          case (split, i) =>
            val currTok = toks(i)
            if (split.optimalAt.contains(tok.left))
              {
                logger.trace(
                    s"""optimal $curr $split ${curr.cost} $tok
                    |${header("output")}
                    |${mkString(curr.splits)}
             """.stripMargin)
                optimal += (state.column -> currTok) -> split
              }
            state = state.next(style, split, currTok)
        }
      }

      def provided(formatToken: FormatToken): Split = {
        // TODO(olafur) the indentation is not correctly set.
        val split =
          Split(Provided(formatToken.between.map(_.code).mkString), 0)
        val result =
          if (formatToken.left.isInstanceOf[`{`])
            split.withIndent(Num(2), parens(formatToken.left), Right)
          else split
        result
      }
      val Q = new mutable.PriorityQueue[State]()
      var result = start
      Q += start
      while ( Q.nonEmpty) {
        val curr = Q.dequeue()
        if (curr.splits.length > deepestYet.splits.length) deepestYet = curr
        explored += 1
        if (explored % 1000 == 0) logger.debug(s"Explored $explored")
        val i = curr.splits.length
        if (explored > MaxVisits || i == toks.length ||
            !childOf(toks(i).right, owner, owners))
          {
            result = curr
            Q.dequeueAll
          }
        else if (!pruneOK(curr))
          {
            val splitToken = toks(i)
            updateOptimal(splitToken, curr)
            Debug.visit(splitToken)
            if (Q.nonEmpty)
              {
                val minCost = Q.minBy(_.cost)
                logger.trace(
                    s"""
                 |visit=$splitToken
                 |lastSplit=${curr.splits.last}
                 |cost=${curr.cost}
                 |minCost=${minCost.cost}
                 |Q.size=${Q.size}
                 |""".stripMargin)
              }
            val splits: List[Split] =
              if (splitToken.insideRange(range)) formatter.Route(splitToken)
              else List(provided(splitToken))
            val actualSplit = curr.policy(Decision(splitToken, splits)).split
            actualSplit.withFilter(! _.ignoreIf).foreach {
              split =>
              val nextState = curr.next(style, split, splitToken)
              if (split.modification == Newline)
                best += splitToken.left -> nextState
              // TODO(olafur) this is a questionable optimization, it introduces
              // a lot of complexity to the search and I'm not still convinced of its
              // usefulness if we design the graph better.
              if (splitToken.left != owner.tokens.head &&
                  startsUnwrappedLine(splitToken.left,
                                      statementStarts,
                                      owners(splitToken.left)))
                {
                  val nextNextState =
                    shortestPathMemo(owners(splitToken.left), nextState, curr)
                  Q.enqueue(nextNextState)
                }
              else { Q.enqueue(nextState) }
            }
          }
      }
      result
    }
    var state = shortestPathMemo(tree, State.start, State.start)
    if (state.splits.length != toks.length)
      {
        state = deepestYet
        logger.warn("UNABLE TO FORMAT")
      }
    Debug.explored += explored
    Debug.state = state
    Debug.tokens = toks
    mkString(state.splits)
  }

  def shouldFormat(range: Range, formatToken: FormatToken): Boolean = {
    range.contains(formatToken.left.position.start.line)
  }

  def startsUnwrappedLine(token: Token, starts: Map[Token, Tree],
          owner: Tree): Boolean = {
    if (starts.contains(token))
      true else if (!owner.tokens.headOption.contains(token))
      false else owner match {
      case _: Defn | _: Case | _: Pkg => true
      case _ => false
    }
  }

  private def getStatementStarts(tree: Tree): Map[Token, Tree] = {
    val ret =
      new mutable.MapBuilder[Token, Tree, Map[Token, Tree]](Map[Token, Tree]())

    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach {
        t =>
        ret += t.tokens.head -> t
      }
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: internal.ast.Source => addAll(t.stats)
        case t: internal.ast.Pkg => addAll(t.stats)
        case t: internal.ast.Term.Match => addAll(t.cases)
        case t: internal.ast.Term.PartialFunction => addAll(t.cases)
        case b: Block =>
          addAll(b.stats)
        // TODO(olafur) Working with templates is really awkward.
        case t: internal.ast.Template if t.stats.isDefined =>
          addAll(t.stats.get)
        case _ => // Nothing
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

  /**
    * Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  private def matchingParens(tokens: Tokens): Map[Token, Token] = {
    val ret = new mutable.MapBuilder[Token, Token, Map[Token, Token]](
        Map.empty[Token, Token])
    var stack = List.empty[Token]
    tokens.foreach {
      case open@( _: `{` | _: `[` | _: `(`) => stack = open :: stack
      case close@( _: `}` | _: `]` | _: `)`) =>
        val open = stack.head
        ret += open -> close
        ret += close -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    assertValidParens(result)
    result
  }

  def assertValidParens(parens: Map[Token, Token]): Unit = {
    parens.foreach {
      case (_: `{`, _: `}`) =>
      case (_: `}`, _: `{`) =>
      case (_: `[`, _: `]`) =>
      case (_: `]`, _: `[`) =>
      case (_: `(`, _: `)`) =>
      case (_: `)`, _: `(`) =>
      case (open, close) =>
        throw new IllegalArgumentException(
            s"Mismatching parens ($open, $close)")
    }
  }

  /**
    * Creates lookup table from token to its closest scala.meta tree.
    */
  private def getOwners(tree: Tree): Map[Token, Tree] = {
    val result = mutable.Map.empty[Token, Tree]

    def loop(x: Tree): Unit = {
      x.tokens.foreach {
        tok =>
        result += tok -> x
      }
      x match {
        case _: Interpolate =>
        // TODO(olafur) the mod is unintuitive
        case _: scala.meta.internal.ast.Mod.Override.Api =>
        // Nothing
        case _ => x.children.foreach(loop)
      }
    }
    loop(tree)
    result.toMap
  }
}

object ScalaFmt {

  def format(code: String, style: ScalaStyle,
          range: Option[Range] = None): String = {
    new ScalaFmt(style).format[scala.meta.Stat](
        code, range.map(r => Range(r.start - 1, r.end).inclusive))
  }
}