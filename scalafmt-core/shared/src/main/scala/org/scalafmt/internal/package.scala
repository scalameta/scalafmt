package org.scalafmt

import scala.language.implicitConversions

package object internal {

  private[scalafmt] type FT = FormatToken
  private[scalafmt] val FT = FormatToken

  private[scalafmt] implicit def implicitModToModExt(
      mod: Modification,
  ): ModExt = ModExt(mod)

  private[scalafmt] implicit class ImplicitModification(
      private val mod: Modification,
  ) extends AnyVal {
    def toExt: ModExt = mod
  }

}
