package scala.fmt

import scala.meta._

object ScalaFmt {
  def procedureSyntax(source: Tree): Tree = {
    val newSource = source.transform {
      case t @ q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" => {
        val result = tpeopt match {
          // Matching against empty syntax may be incorrect in some cases,
          // but I can't come up with a failing example.
          case Some(tpe) if tpe.show[Syntax] == "" =>
            q"..$mods def $name[..$tparams](...$paramss): Unit = $expr"
          case _ => t
        }
        result
      }
    }
    // transform returns Any, in what cases does it not return subtype of Tree?
    newSource.asInstanceOf[Tree]
  }

  def main(args: Array[String]): Unit = {
    ???
  }
}
