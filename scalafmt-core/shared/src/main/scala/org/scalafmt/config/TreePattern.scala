package org.scalafmt.config

import org.scalafmt.util.ParamClauseParent

import java.util.regex.{Pattern => jurPattern}

import metaconfig._

/** @param regex
  *   regexp for class name of scala.meta.Tree "owner".
  * @param parents
  *   optional regexp for class name of owner's parent.
  */
case class TreePattern(
    regex: Option[String] = None,
    parents: Seq[String] = Seq.empty,
) {
  def getMatcher: TreePattern.Matcher = new TreePattern.Matcher(this)
}

object TreePattern {
  val default = TreePattern()
  implicit val surface: generic.Surface[TreePattern] = generic
    .deriveSurface[TreePattern]
  implicit val codec: ConfCodecEx[TreePattern] = generic.deriveCodecEx(default)

  private def pattern(value: String): jurPattern = value.r.pattern

  class Matcher(val owner: Option[jurPattern], val parents: Seq[jurPattern]) {
    def this(obj: TreePattern) =
      this(obj.regex.map(pattern), obj.parents.map(pattern))
    def matches(tree: meta.Tree): Boolean = owner.forall(check(tree)) &&
      (parents.isEmpty || tree.parent.exists(p =>
        parents.forall(check(p)) ||
          (p match {
            case ParamClauseParent(pp) => parents.forall(check(pp))
            case _: meta.Member.SyntaxValuesClause => p.parent
                .exists(pp => parents.forall(check(pp)))
            case _ => false
          }),
      ))
    def isEmpty: Boolean = owner.isEmpty && parents.isEmpty
  }

  @inline
  private def check(tree: meta.Tree)(pattern: jurPattern): Boolean = pattern
    .matcher(tree.productPrefix).find()

}
