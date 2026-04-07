package org.scalafmt.dynamic

import org.scalafmt.interfaces._

import munit.FunSuite

class DynamicSuite extends FunSuite {

  test("Scalafmt interface")(
    interceptMessage[ScalafmtException](
      "Can't use different version for native CLI",
    )(Scalafmt.create(this.getClass.getClassLoader)),
  )

}
