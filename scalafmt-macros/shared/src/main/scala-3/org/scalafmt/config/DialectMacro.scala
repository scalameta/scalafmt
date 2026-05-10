package org.scalafmt.config

import scala.meta.Dialect

import scala.language.experimental.macros
import scala.quoted.*

private[scalafmt] object DialectMacro {
  import DialectMap.MapType

  inline def dialectMap: MapType = ${ dialectMapImpl }

  private def dialectMapImpl(using Quotes): Expr[MapType] = {
    import quotes.reflect.*
    val dtype = TypeRepr.of[Dialect]
    val entries = dtype.typeSymbol.memberMethods.iterator.flatMap { method =>
      val name = method.name
      def isMatching = method.privateWithin.isEmpty &&
        !method.flags.is(Flags.Private) && !method.flags.is(Flags.Protected) &&
        name.startsWith("with")
      method.paramSymss match {
        case (param :: Nil) :: Nil if isMatching =>
          dtype.memberType(param).asType match {
            case '[t] =>
              val fn: Expr[DialectMap.ValType] =
                '{ (dialect: Dialect, v: Any) =>
                  ${
                    Apply(
                      Select.unique('dialect.asTerm, name),
                      List('{ v.asInstanceOf[t] }.asTerm),
                    ).asExprOf[Dialect]
                  }
                }
              Some('{ ${ Expr(name) } -> $fn })
            case _ => None
          }
        case _ => None
      }
    }

    '{ Map.from(${ Expr.ofList(entries.toList) }) }
  }

}
