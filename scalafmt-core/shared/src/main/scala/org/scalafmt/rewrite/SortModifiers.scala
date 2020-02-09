package org.scalafmt.rewrite

import org.scalafmt.config.SortSettings._
import org.scalafmt.util.TreeOps

import scala.meta._

object SortModifiers extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new SortModifiers
}

class SortModifiers(implicit ctx: RewriteCtx) extends RewriteSession {

  private implicit val order = ctx.style.rewrite.sortModifiers.order

  override def rewrite(tree: Tree): Unit = tree match {

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
    case d: Decl.Def => sortMods(d.mods)
    case v: Decl.Val => sortMods(v.mods)
    case v: Decl.Var => sortMods(v.mods)
    case t: Decl.Type => sortMods(t.mods)
    case d: Defn.Def => sortMods(d.mods)
    case v: Defn.Val => sortMods(v.mods)
    case v: Defn.Var => sortMods(v.mods)
    case t: Defn.Type => sortMods(t.mods)
    case c: Defn.Class => sortMods(c.mods.filterNot(_.is[Mod.Case]))
    case o: Defn.Object => sortMods(o.mods.filterNot(_.is[Mod.Case]))
    case t: Defn.Trait => sortMods(t.mods)
    case p: Term.Param =>
      sortMods(
        p.mods.filterNot(m => m.is[Mod.ValParam] || m.is[Mod.VarParam])
      )
    case _ =>
  }

  private def sortMods(oldMods: Seq[Mod]): Unit = {
    if (oldMods.nonEmpty) {
      // ignore implicit "implicit" whenever we sort, and apply patches
      val sanitized = oldMods.filterNot(TreeOps.isHiddenImplicit)
      val sortedMods: Seq[Mod] = sanitized.sortWith(orderModsBy)

      ctx.addPatchSet(sortedMods.zip(sanitized).flatMap {
        case (next, old) =>
          if (next eq old) Seq.empty
          else {
            val removeOld = old.tokens.tail.map(TokenPatch.Remove)
            val nextSyntax = next.tokens.syntax // XXX: not next.syntax!
            val addNext = TokenPatch.Replace(old.tokens.head, nextSyntax)
            removeOld :+ addNext
          }
      }: _*)
    }
  }

  /**
    * @return
    *   m1 < m2; according to the order given by the List
    */
  private def orderModsBy(m1: Mod, m2: Mod): Boolean = {
    val idx1 = order.indexWhere(modCorrespondsToSettingKey(m1))
    val idx2 = order.indexWhere(modCorrespondsToSettingKey(m2))
    idx1 < idx2
  }

  private def modCorrespondsToSettingKey(m: Mod)(p: ModKey): Boolean = {
    p == `private` && m.is[Mod.Private] ||
    p == `protected` && m.is[Mod.Protected] ||
    p == `final` && m.is[Mod.Final] ||
    p == `sealed` && m.is[Mod.Sealed] ||
    p == `abstract` && m.is[Mod.Abstract] ||
    p == `lazy` && m.is[Mod.Lazy] ||
    p == `implicit` && m.is[Mod.Implicit] ||
    p == `override` && m.is[Mod.Override]
  }

}
