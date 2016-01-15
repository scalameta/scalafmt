package org

package object scalafmt {

  type Strategy = PartialFunction[FormatToken, List[Split]]
  val EmptyStrategy = PartialFunction.empty[FormatToken, List[Split]]

}
