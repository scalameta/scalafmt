/* Modified version of
https://github.com/akka/akka/blob/c294a22d22adc0c803e1f4183874c13aade2a449/akka-actor/src/main/scala/akka/actor/ReflectiveDynamicAccess.scala

Original licence:

Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */
package org.scalafmt.sbt

import java.lang.reflect.InvocationTargetException

import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Original:
  * https://github.com/akka/akka/blob/master/akka-actor/src/main/scala/akka/actor/ReflectiveDynamicAccess.scala
  *
  */
class ReflectiveDynamicAccess(val classLoader: ClassLoader) {

  def getClassFor[T : ClassTag](fqcn: String): Try[Class[_ <: T]] =
    Try[Class[_ <: T]]({
      val c =
        Class.forName(fqcn, false, classLoader).asInstanceOf[Class[_ <: T]]
      val t = implicitly[ClassTag[T]].runtimeClass
      if (t.isAssignableFrom(c)) c
      else throw new ClassCastException(s"$t is not assignable from $c")
    })

  def createInstanceFor[T : ClassTag](
      clazz: Class[_], args: immutable.Seq[(Class[_], AnyRef)]): Try[T] =
    Try {
      val types = args.map(_._1).toArray
      val values = args.map(_._2).toArray
      val constructor = clazz.getDeclaredConstructor(types: _*)
      constructor.setAccessible(true)
      val obj = constructor.newInstance(values: _*)
      val t = implicitly[ClassTag[T]].runtimeClass
      if (t.isInstance(obj)) obj.asInstanceOf[T]
      else
        throw new ClassCastException(
            s"${clazz.getName} is not a subtype of $t")
    } recover {
      case i: InvocationTargetException if i.getTargetException ne null ⇒
        throw i.getTargetException
    }

  def createInstanceFor[T : ClassTag](
      fqcn: String, args: immutable.Seq[(Class[_], AnyRef)]): Try[T] =
    getClassFor(fqcn) flatMap { c ⇒
      createInstanceFor(c, args)
    }
}
