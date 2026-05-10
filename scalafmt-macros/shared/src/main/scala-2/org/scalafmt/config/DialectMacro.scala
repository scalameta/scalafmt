package org.scalafmt.config

import scala.meta.Dialect

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

// Builds a map between string (the scalafmt method name)
// and dialect method application
private[scalafmt] object DialectMacro {
  import DialectMap.MapType

  def dialectMap: MapType = macro dialectMap_impl

  def dialectMap_impl(c: blackbox.Context): c.Expr[MapType] = {
    import c.universe._
    val methods = typeOf[Dialect].members.flatMap {
      case v: MethodSymbol => v.paramLists match {
          case (param :: Nil) :: Nil if v.isPublic => // single parameter
            val methodName = v.name
            val methodNameStr = methodName.toString
            if (methodNameStr.startsWith("with")) {
              val tpe = param.typeSignature
              Some(q"$methodNameStr -> ((dialect: scala.meta.Dialect, v: Any) => dialect.$methodName(v.asInstanceOf[$tpe]))")
            } else None
          case _ => None
        }
      case _ => None
    }
    c.Expr[MapType](q"""scala.collection.immutable.Map(..$methods)""")
  }
}
