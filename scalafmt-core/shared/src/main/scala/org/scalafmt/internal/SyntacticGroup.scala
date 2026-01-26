package org.scalafmt.internal

sealed trait SyntacticGroup {
  def categories: List[String]
  def precedence: Double
}

object SyntacticGroup {
  sealed trait Type extends SyntacticGroup {
    def categories = List("Type")
  }
  object Type {
    case object ParamTyp extends Type {
      def precedence = 0
    }
    case object Typ extends Type {
      def precedence = 1
    }
    case object AnyInfixTyp extends Type {
      def precedence = 1.5
    }
    case class InfixTyp(operator: String) extends Type {
      def precedence = 2
    }
    case object RefineTyp extends Type {
      def precedence = 3
    }
    case object WithTyp extends Type {
      def precedence = 3.5
    }
    case object AnnotTyp extends Type {
      def precedence = 4
    }
    case object SimpleTyp extends Type {
      def precedence = 6
    }
  }
  sealed trait Term extends SyntacticGroup {
    def categories = List("Term")
  }
  object Term {
    case object Expr extends Term {
      def precedence = 0
    }
    case object Expr1 extends Term {
      def precedence = 1
    }
    case object Ascription extends Term {
      def precedence = 2
    }
    case object PostfixExpr extends Term {
      def precedence = 2
    }
    case class InfixExpr(operator: String) extends Term {
      def precedence = 3
    }
    case class PrefixExpr(operator: String) extends Term {
      def precedence = 4
    }
    case object SimpleExpr extends Term {
      def precedence = 5
    }
    case object SimpleExpr1 extends Term {
      def precedence = 6
    }
  }
  sealed trait Pat extends SyntacticGroup {
    def categories = List("Pat")
  }
  object Pat {
    case object Pattern extends Pat {
      def precedence = 0
    }
    case object Pattern1 extends Pat {
      def precedence = 1
    }
    case object Pattern2 extends Pat {
      def precedence = 2
    }
    case object AnyPattern3 extends Pat {
      def precedence = 2.5
    }
    case class Pattern3(operator: String) extends Pat {
      def precedence = 3
    }
    case object SimplePattern extends Pat {
      def precedence = 6
    }
  }
  case object Literal extends Term with Pat {
    override def categories = List("Term", "Pat"); def precedence = 6
  }
  require(
    Literal.precedence == Term.SimpleExpr1.precedence &&
      Literal.precedence == Pat.SimplePattern.precedence,
  )
  case object Path extends Type with Term with Pat {
    override def categories = List("Type", "Term", "Pat")
    def precedence = 6
  }
  require(
    Path.precedence == Type.SimpleTyp.precedence &&
      Path.precedence == Term.SimpleExpr1.precedence &&
      Path.precedence == Pat.SimplePattern.precedence,
  )
}
