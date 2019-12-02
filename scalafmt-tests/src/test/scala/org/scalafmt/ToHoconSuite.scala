package org.scalafmt

import metaconfig.Conf
import metaconfig.ConfEncoder
import org.scalafmt.config.Config
import org.scalafmt.util.DiffAssertions
import org.scalatest.funsuite.AnyFunSuite

class ConfDiffSuite extends AnyFunSuite with DiffAssertions {}
class ToHoconSuite extends AnyFunSuite with DiffAssertions {}
