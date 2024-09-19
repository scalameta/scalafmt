package org.scalafmt.dynamic.exceptions

import java.lang.reflect.InvocationTargetException

object ReflectionException {
  def unapply(e: Throwable): Option[Throwable] = Some(flatten(e))

  def flatten(e: Throwable): Throwable = e match {
    case e: InvocationTargetException => e.getCause
    case _ => e
  }
}
