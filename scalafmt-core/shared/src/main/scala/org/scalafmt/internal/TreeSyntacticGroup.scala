package org.scalafmt.internal

import scala.{meta => m}

object TreeSyntacticGroup {
  import m.internal.prettyprinters.{TreeSyntacticGroup => SyntacticGroup}
  import SyntacticGroup._

  def apply(tree: m.Tree): SyntacticGroup = tree match {
    case _: m.Lit => Literal
    // Term
    case _: m.Term.Name => Path
    case _: m.Term.Select => Path
    case _: m.Term.SelectPostfix => PostfixExpr
    case _: m.Term.Interpolate => SimpleExpr1
    case _: m.Term.Xml => SimpleExpr1
    case _: m.Term.Apply => SimpleExpr1
    case _: m.Term.ApplyType => SimpleExpr1
    case _: m.Term.SelectMatch => SimpleExpr1
    case t: m.Term.ApplyInfix => InfixExpr(t)
    case _: m.Term.ApplyUnary => PrefixExpr
    case _: m.Term.Assign => Expr1
    case _: m.Term.Return => Expr1
    case _: m.Term.Throw => Expr1
    case _: m.Term.Ascribe => Expr1
    case _: m.Term.Annotate => Expr1
    case _: m.Term.Block => SimpleExpr1
    case _: m.Term.Tuple => SimpleExpr1 // ???, breaks a op ((b, c))
//    case _: m.Term.Tuple => Expr1 // ??? Was SimpleExpr1, which is buggy for `a op ((b, c))
    case _: m.Term.If => Expr1
    case _: m.Term.Match => Expr1
    case _: m.Term.TryClause => Expr1
    case _: m.Term.FunctionTerm => Expr
    case _: m.Term.PolyFunction => Expr
    case _: m.Term.PartialFunction => SimpleExpr
    case _: m.Term.While => Expr1
    case _: m.Term.Do => Expr1
    case _: m.Term.ForClause => Expr1
    case _: m.Term.New => SimpleExpr
    case _: m.Term.Placeholder => SimpleExpr1
    case _: m.Term.Eta => SimpleExpr
    case _: m.Term.Repeated => PostfixExpr
    case _: m.Term.Param => Path // ???
    // Type
    case _: m.Type.Name => Path
    case _: m.Type.TypedParam => SimpleTyp
    case _: m.Type.Select => SimpleTyp
    case _: m.Type.Project => SimpleTyp
    case _: m.Type.Singleton => SimpleTyp
    case _: m.Type.Apply => SimpleTyp
    case t: m.Type.ApplyInfix => InfixTyp(t)
    case _: m.Type.ParamFunctionType => Typ
    case _: m.Type.PolyFunction => Typ
    case _: m.Type.Tuple => SimpleTyp
    case _: m.Type.With => WithTyp
    case _: m.Type.Refine => RefineTyp
    case _: m.Type.Existential => Typ
    case _: m.Type.Annotate => AnnotTyp
    case _: m.Type.Lambda => Typ
    case _: m.Type.AnonymousParam => SimpleTyp
    case _: m.Type.Wildcard => SimpleTyp
    case _: m.Type.Bounds => Path // ???
    case _: m.Type.Repeated => ParamTyp
    case _: m.Type.ByNameType => ParamTyp
    case _: m.Type.Var => ParamTyp
    case _: m.Type.Param => Path // ???
    case _: m.Type.Match => Typ
    // Pat
    case _: m.Pat.Var => SimplePattern
    case _: m.Pat.Wildcard => SimplePattern
    case _: m.Pat.SeqWildcard => SimplePattern
    case _: m.Pat.Bind => Pattern2
    case _: m.Pat.Alternative => Pattern
    case _: m.Pat.Tuple => SimplePattern
    case _: m.Pat.Extract => SimplePattern
    case t: m.Pat.ExtractInfix => InfixPat(t)
    case _: m.Pat.Interpolate => SimplePattern
    case _: m.Pat.Xml => SimplePattern
    case _: m.Pat.Typed => Pattern1

    // Misc
    case _ => Path
  }

  def groupNeedsParens(outer: m.Tree, inner: m.Tree)(implicit
      dialect: m.Dialect,
  ): Boolean = SyntacticGroup.groupNeedsParens(apply(outer), apply(inner))

}
