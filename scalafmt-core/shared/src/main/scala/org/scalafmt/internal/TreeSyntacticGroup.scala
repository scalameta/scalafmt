package org.scalafmt.internal

import org.scalafmt.internal.{SyntacticGroup => g}

import scala.meta.Lit
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type

object TreeSyntacticGroup {
  def apply(tree: Tree): SyntacticGroup = tree match {
    case _: Lit => g.Literal
    // Term
    case _: Term.Name => g.Path
    case _: Term.Select => g.Path
    case _: Term.SelectPostfix => g.Term.PostfixExpr
    case _: Term.Interpolate => g.Term.SimpleExpr1
    case _: Term.Xml => g.Term.SimpleExpr1
    case _: Term.Apply => g.Term.SimpleExpr1
    case _: Term.ApplyType => g.Term.SimpleExpr1
    case _: Term.SelectMatch => g.Term.SimpleExpr1
    case t: Term.ApplyInfix => g.Term.InfixExpr(t.op.value)
    case t: Term.ApplyUnary => g.Term.PrefixExpr(t.op.value)
    case _: Term.Assign => g.Term.Expr1
    case _: Term.Return => g.Term.Expr1
    case _: Term.Throw => g.Term.Expr1
    case _: Term.Ascribe => g.Term.Expr1
    case _: Term.Annotate => g.Term.Expr1
    case _: Term.Block => g.Term.SimpleExpr1
    case _: Term.Tuple => g.Term.SimpleExpr1 // ???, breaks a op ((b, c))
//    case _: Term.Tuple => g.Term.Expr1 // ??? Was SimpleExpr1, which is buggy for `a op ((b, c))
    case _: Term.If => g.Term.Expr1
    case _: Term.Match => g.Term.Expr1
    case _: Term.TryClause => g.Term.Expr1
    case _: Term.FunctionTerm => g.Term.Expr
    case _: Term.PolyFunction => g.Term.Expr
    case _: Term.PartialFunction => g.Term.SimpleExpr
    case _: Term.While => g.Term.Expr1
    case _: Term.Do => g.Term.Expr1
    case _: Term.ForClause => g.Term.Expr1
    case _: Term.New => g.Term.SimpleExpr
    case _: Term.Placeholder => g.Term.SimpleExpr1
    case _: Term.Eta => g.Term.SimpleExpr
    case _: Term.Repeated => g.Term.PostfixExpr
    case _: Term.Param => g.Path // ???
    // Type
    case _: Type.Name => g.Path
    case _: Type.TypedParam => g.Type.SimpleTyp
    case _: Type.Select => g.Type.SimpleTyp
    case _: Type.Project => g.Type.SimpleTyp
    case _: Type.Singleton => g.Type.SimpleTyp
    case _: Type.Apply => g.Type.SimpleTyp
    case t: Type.ApplyInfix => g.Type.InfixTyp(t.op.value)
    case _: Type.ParamFunctionType => g.Type.Typ
    case _: Type.PolyFunction => g.Type.Typ
    case _: Type.Tuple => g.Type.SimpleTyp
    case _: Type.With => g.Type.WithTyp
    case _: Type.Refine => g.Type.RefineTyp
    case _: Type.Existential => g.Type.Typ
    case _: Type.Annotate => g.Type.AnnotTyp
    case _: Type.Lambda => g.Type.Typ
    case _: Type.AnonymousParam => g.Type.SimpleTyp
    case _: Type.Wildcard => g.Type.SimpleTyp
    case _: Type.Bounds => g.Path // ???
    case _: Type.Repeated => g.Type.ParamTyp
    case _: Type.ByNameType => g.Type.ParamTyp
    case _: Type.Var => g.Type.ParamTyp
    case _: Type.Param => g.Path // ???
    case _: Type.Match => g.Type.Typ
    // Pat
    case _: Pat.Var => g.Pat.SimplePattern
    case _: Pat.Wildcard => g.Pat.SimplePattern
    case _: Pat.SeqWildcard => g.Pat.SimplePattern
    case _: Pat.Bind => g.Pat.Pattern2
    case _: Pat.Alternative => g.Pat.Pattern
    case _: Pat.Tuple => g.Pat.SimplePattern
    case _: Pat.Extract => g.Pat.SimplePattern
    case t: Pat.ExtractInfix => g.Pat.Pattern3(t.op.value)
    case _: Pat.Interpolate => g.Pat.SimplePattern
    case _: Pat.Xml => g.Pat.SimplePattern
    case _: Pat.Typed => g.Pat.Pattern1

    // Misc
    case _ => g.Path
  }
}
