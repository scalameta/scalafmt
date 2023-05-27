package org.scalafmt.rewrite

import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.TreeOps

import scala.meta._

object SortModifiers extends RewriteFactory {

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v2.sortModifiers ne v1.sortModifiers

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new SortModifiers
}

class SortModifiers(implicit ctx: RewriteCtx) extends RewriteSession {

  private implicit val order = ctx.style.rewrite.sortModifiers.order

  override def rewrite(tree: Tree): Unit =
    tree match {

      /*
       * in the case of Class, Object, and of class constructor parameters
       * some Mods are immovable, e.g. 'case' in "case class X".
       *
       * The case of parameters is a bit more curious because there the
       * "val" or "var" in, say:
       * {{{
       *   class Test(private final val x: Int)
       * }}}
       * are considered Mods, instead of being similar to `Defn.Val`, or `Defn.Var`.
       */
      case c: Defn.Class => sortMods(c.mods.filterNot(_.is[Mod.Case]))
      case o: Defn.Object => sortMods(o.mods.filterNot(_.is[Mod.Case]))
      case s: Stat.WithMods => sortMods(s.mods)
      case p: Term.Param =>
        val start = p.pos.start
        sortMods(p.mods.filterNot { m =>
          m.is[Mod.ValParam] || m.is[Mod.VarParam] || m.is[Mod.Using] ||
          TreeOps.noExplicitImplicit(start, false)(m)
        })
      case _ =>
    }

  private def sortMods(oldMods: Seq[Mod]): Unit = {
    if (oldMods.nonEmpty) {
      val sanitized = oldMods // used to ignore "implicit" implicits
      // NOTE: modifiers with no configuration return -1 from `indexWhere` and
      // therefore sort at the front of the list. This behavior is intentional
      // in order to preserve backwards compatibility when adding support to
      // format new keywords. However, the choice of putting unconfigured
      // modifiers to the front of the list instead of back of the list is
      // mostly arbitrary.
      val sortedMods: Seq[Mod] =
        sanitized.sortBy(mod => order.indexWhere(_.matches(mod)))

      ctx.addPatchSet(sortedMods.zip(sanitized).flatMap { case (next, old) =>
        if (next eq old) Seq.empty
        else {
          val removeOld = old.tokens.tail.map(TokenPatch.Remove)
          val addNext = TokenPatch.Replace(old.tokens.head, next.toString())
          addNext +: removeOld
        }
      }: _*)
    }
  }

}
