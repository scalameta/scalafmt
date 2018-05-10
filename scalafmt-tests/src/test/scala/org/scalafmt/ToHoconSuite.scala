package org.scalafmt

import metaconfig.Conf
import metaconfig.ConfEncoder
import org.scalafmt.config.Config
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class ConfDiffSuite extends FunSuite with DiffAssertions {}
class ToHoconSuite extends FunSuite with DiffAssertions {}
