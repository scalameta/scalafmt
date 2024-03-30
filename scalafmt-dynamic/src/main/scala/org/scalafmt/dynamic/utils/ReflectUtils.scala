package org.scalafmt.dynamic.utils

import scala.reflect.ClassTag

object ReflectUtils {

  implicit class ImplicitAnyRef[T <: AnyRef](val target: T) extends AnyVal {
    def invokeAs[R](methodName: String, args: (Class[_], AnyRef)*): R =
      invoke(methodName, args: _*).asInstanceOf[R]

    def invoke(methodName: String, args: (Class[_], AnyRef)*): AnyRef = target
      .getClass.invokeStaticOn(target, methodName, args: _*)
  }

  implicit class ImplicitAny[T](val target: T) extends AnyVal {
    def asParam(implicit classTag: ClassTag[T]): (Class[_], AnyRef) =
      asParam(classTag.runtimeClass)

    def asParam(clazz: Class[_]): (Class[_], AnyRef) =
      (clazz, target.asInstanceOf[AnyRef])
  }

  implicit class ImplicitClass(val clazz: Class[_]) extends AnyVal {
    def invokeStaticAs[T](methodName: String, args: (Class[_], AnyRef)*): T =
      invokeStatic(methodName, args: _*).asInstanceOf[T]

    def invokeStatic(methodName: String, args: (Class[_], AnyRef)*): AnyRef =
      invokeStaticOn(null, methodName, args: _*)

    def invokeStaticOn(
        target: AnyRef,
        methodName: String,
        args: (Class[_], AnyRef)*
    ): AnyRef = {
      val method = clazz.getMethod(methodName, args.map(_._1): _*)
      method.invoke(target, args.map(_._2): _*)
    }
  }

}
