package org.scalafmt.internal

import scala.meta._

import scala.collection.mutable

class OptimizationEntities(
    argumentStarts: Map[Int, Tree],
    optionalNewlines: Set[Int],
) {
  def argumentAt(idx: Int): Option[Tree] = argumentStarts.get(idx)
  def argument(implicit ft: FormatToken): Option[Tree] = argumentAt(ft.meta.idx)
  def optionalNL(implicit ft: FormatToken): Boolean =
    optionalNewlines(ft.meta.idx)
}

object OptimizationEntities {
  def apply(tree: Tree)(implicit ftoks: FormatTokens): OptimizationEntities =
    new Builder(tree).build()

  private class Builder(topSourceTree: Tree)(implicit tokens: FormatTokens) {

    private val arguments = mutable.Map.empty[Int, Tree]
    private val optional = Set.newBuilder[Int]

    def build(): OptimizationEntities = {
      val queue = new mutable.ListBuffer[Seq[Tree]]
      queue += topSourceTree :: Nil
      while (queue.nonEmpty) queue.remove(0).foreach { tree =>
        processForArguments(tree)
        queue += tree.children
      }
      new OptimizationEntities(arguments.toMap, optional.result())
    }

    private def getHeadIndex(tree: Tree): Option[Int] = tokens.getHeadOpt(tree)
      .map(_.meta.idx - 1)
    private def addArgWith(key: Tree)(value: Tree): Unit = getHeadIndex(key)
      .foreach(arguments.getOrElseUpdate(_, value))
    private def addArg(tree: Tree): Unit = addArgWith(tree)(tree)
    private def addOptional(tree: Tree): Unit = getHeadIndex(tree)
      .foreach(optional += _)
    private def addParam(t: Term.Param, key: Tree): Unit = {
      addArgWith(key)(t)
      t.mods.foreach(addOptional)
      addOptional(t.name)
    }

    private def processForArguments(tree: Tree): Unit = tree match {
      case _: Lit.Unit =>
      case t: Term.ParamClause =>
        val params = t.mod match {
          case Some(mod) =>
            addOptional(mod)
            t.values match {
              case head :: rest =>
                addParam(head, mod)
                rest
              case _ => Nil
            }
          case _ => t.values
        }
        params.foreach(x => addParam(x, x))
      case t: Term.ArgClause => addArg(t)
      case t: Member.SyntaxValuesClause => t.values.foreach(addArg)
      case t: Member.Tuple => t.args.foreach(addArg)
      case _: Term.Param => // covered by Term.ParamClause
      case t: Term => addArg(t)
      case _ =>
    }
  }

}
