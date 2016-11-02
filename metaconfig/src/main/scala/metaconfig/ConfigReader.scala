package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable.Map
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Constant
import scala.reflect.ClassTag

class Error(msg: String) extends Exception(msg)
case class ConfigError(msg: String) extends Error(msg)
case class ConfigErrors(es: Seq[Throwable])
    extends Error(s"Errors: ${es.mkString("\n")}")

class ExtraName(string: String) extends scala.annotation.StaticAnnotation

@compileTimeOnly("@metaconfig.Config not expanded")
class ConfigReader extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def genReader(typ: Type, params: Seq[Term.Param] = Seq.empty): Defn.Val = {
      val mapName = Term.Name("map")
      val classLit = Lit(typ.syntax)
      val extraNames: Map[String, Seq[Term.Arg]] = params.collect {
        case p: Term.Param =>
          p.name.syntax -> p.mods.collect {
            case mod"@ExtraName(..${List(extraName)})" => extraName
            case mod"@metaconfig.ExtraName(..${List(extraName)})" => extraName
          }
      }.toMap
      def defaultArgs: Seq[Term.Arg] = {
        params.collect {
          case Term.Param(mods, pName: Term.Name, Some(pTyp: Type), _) =>
            val nameLit = Lit(pName.syntax)
            val args = Seq(pName, nameLit) ++ extraNames(pName.syntax)
            Term.Arg.Named(
              pName,
              q"""_root_.metaconfig.Metaconfig.get[$pTyp](
                    $mapName, $classLit)(..$args)"""
            )
        }
      }
      val argLits =
        params.map(x => Lit(x.name.syntax)) ++
          extraNames.values.flatten
      val constructor = Ctor.Ref.Name(typ.syntax)
      val bind = Term.Name("x")
      val patTyped = Pat.Typed(Pat.Var.Term(bind), typ.asInstanceOf[Pat.Type])
      q"""val reader = new _root_.metaconfig.Reader[$typ] {
          override def read(any: Any): _root_.metaconfig.Result[$typ] = {
            any match {
              case ($patTyped) => Right($bind)
              case _root_.metaconfig.String2AnyMap(${Pat.Var.Term(mapName)}) =>
                val validFields = _root_.scala.collection.immutable.Set(..$argLits)
                val invalidFields = $mapName.keys.filterNot(validFields)
                if (invalidFields.nonEmpty) {
                  val msg =
                    "Error reading class '" + $classLit + "'. " +
                    "Invalid fields: " + invalidFields.mkString(", ")
                  Left(_root_.metaconfig.ConfigError(msg))
                } else {
                  try {
                      Right(new $constructor(..$defaultArgs))
                  } catch {
                    case _root_.scala.util.control.NonFatal(e) => Left(e)
                  }
                }
              case els =>
                val msg =
                  $classLit + " cannot be '" + els +
                    "' (of class " + els.getClass.getSimpleName + ")."
                Left(_root_.metaconfig.ConfigError(msg))
            }
          }
        }
     """
    }

    def expandClass(c: Defn.Class): Stat = {
      val q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" =
        c
      val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" =
        template

      // TODO(olafur) come up with a way to avoid inheritance :/
      val newCtorCalls: Seq[Ctor.Call] =
        ctorcalls :+ Ctor.Ref.Name("_root_.metaconfig.HasFields")
      val flatParams = paramss.flatten
      val fields: Seq[Term.Tuple] = flatParams.collect {
        case Term.Param(_, name: Term.Name, _, _) =>
          q"(${Lit(name.syntax)}, $name)"
      }
      val fieldsDef: Stat = {
        val body =
          Term.Apply(q"_root_.scala.collection.immutable.Map", fields)
        q"def fields: Map[String, Any] = $body"
      }
      val typReader = genReader(tname, flatParams)
      val newStats = stats ++ Seq(fieldsDef) ++ Seq(typReader)
      val newTemplate = template"""
        { ..$earlyStats } with ..$newCtorCalls { $param => ..$newStats }
                                  """
      val result =
        q"""
            ..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $newTemplate
         """
      result
    }

    defn match {
      case c: Defn.Class => expandClass(c)
      case Term.Block(Seq(c: Defn.Class, companion)) =>
        q"""
        ${expandClass(c)}; $companion
         """
      case els =>
        abort(s"Failed to expand: ${els.structure}")
    }
  }
}
