package org.scalafmt.cli.difflib

import java.util

trait DiffAlgorithm[T] {
  def diff(original: util.List[T], revised: util.List[T]): Patch[T]
}
