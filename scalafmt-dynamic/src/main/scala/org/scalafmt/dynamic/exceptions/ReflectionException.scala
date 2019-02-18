package org.scalafmt.dynamic.exceptions

import java.lang.reflect.InvocationTargetException

object ReflectionException {
  def unapply(e: Throwable): Option[Throwable] = e match {
    case e: InvocationTargetException => Some(e.getCause)
    case _ => Some(e)
  }
}
