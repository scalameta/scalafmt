package org.scalafmt.rewrite

import org.scalafmt.config.SortSettings._

import scala.meta.Tree
import scala.meta._

object SortModifiers extends Rewrite {

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val order = ctx.style.rewrite.sortModifiers.order

    val sortMods: Seq[Mod] => Seq[Mod] = { mods =>
      mods.sortWith(orderModsBy(order))
    }

    /*
     * in the case of Class, Object, and of class constructor parameters
     * some Mods are immovable, e.g. "case class X".
     *
     * The case of parameters is a bit more curious, because there the
     * "val" or "var" in, say:
     * {{{
     *   class Test(private final val x: Int)
     * }}}
     * are considered Mods, instead of being similar to `Defn.Val`, or `Defn.Var`.
     */
    val patchesOfPatches = code.collect {
      case d: Defn.Def => patchMods(sortMods, d.mods)
      case v: Defn.Val => patchMods(sortMods, v.mods)
      case v: Defn.Var => patchMods(sortMods, v.mods)
      case t: Defn.Type => patchMods(sortMods, t.mods)
      case c: Defn.Class => patchMods(sortMods, c.mods.filterNot(isCase))
      case o: Defn.Object => patchMods(sortMods, o.mods.filterNot(isCase))
      case t: Defn.Trait => patchMods(sortMods, t.mods)
      case p: Term.Param => patchMods(sortMods, p.mods.filterNot(isValOrVar))
    }
    patchesOfPatches.flatten
  }

  private val isValOrVar: Mod => Boolean = m =>
    m.is[Mod.ValParam] || m.is[Mod.VarParam]

  private val isCase: Mod => Boolean = m => m.is[Mod.Case]

  private def patchMods(
      sortMods: Seq[Mod] => Seq[Mod],
      oldMods: Seq[Mod]): Seq[Patch] = {
    if (oldMods.isEmpty) Nil
    else {
      val sortedMods: Seq[Mod] = sortMods(oldMods)
      sortedMods.zip(oldMods).flatMap {
        case (next, old) =>
          if (old.tokens.isEmpty) {
            //Not sure why this happens, how can you have a mod with no tokens?
            //but see the `SortModifiers_Mod_With_No_Token.source` test for an example.
            //The weirdest part is that it actually formats the file correctly...
            //
            //so it's better to not do anything than crash `scalafmt`
            //P.S. can I emmit a warning?
            Nil
          } else {
            val removeOld = old.tokens.map(t => TokenPatch.Remove(t))
            val addNext = TokenPatch.AddRight(old.tokens.head, next.syntax)
            removeOld :+ addNext
          }
      }
    }
  }

  /**
    * @return
    *   m1 < m2; according to the order given by the Vector
    */
  private def orderModsBy(order: Vector[ModKey])(m1: Mod, m2: Mod): Boolean = {
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
