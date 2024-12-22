package com.lox.parsing

object PrettyPrinterSimpleParseTree {
  import ExpressionSimplified._

  def apply(expression: ParseToken): String = expression {
    case Program(statements) =>
      val strings = statements.map(apply)
      strings.mkString("\n")

    case statement: Statement => statement match {
      case Statement.Print(expression) => parenthesize("print", expression)
      case Statement.SExpression(expression) => apply(expression)
      case Statement.Var(identifier, expression) => s"(let $identifier ${apply(expression)})"
      case Statement.IfElse(cond, statement1, statement2) => s"(if ${apply(cond)} ${apply(statement1)} ${apply(statement2)})"
      case Statement.While(cond, statement) => s"(while ${apply(cond)} ${apply(statement)})"
      case Statement.Block(expressions) => s"{\n${expressions.map(apply).mkString("\n")}\n}"
    }

    case Logical(left, operator, right) =>
      parenthesize(operator.token.lexeme, left, right)
    case Assignment(identifier, expression) =>
      s"(set $identifier ${apply(expression)})"
    case Variable(variable) =>
      variable.toString
    case Binary(left, operator, right) =>
      parenthesize(operator.token.lexeme, left, right)
    case Grouping(expression) =>
      parenthesize("group", expression)
    case literal: Literal => literal.toString
    case Unary(operator, expression) =>
      parenthesize(operator.token.lexeme, expression)
  }

  def parenthesize(name: String, exprs: Expression*) = {
    val subExprs: Seq[String] = exprs.map(apply)
    val exprStrings = subExprs.mkString(" ")
    f"($name $exprStrings)"
  }
}

object PrettyPrinterParseTree {
  import ParseToken._

  def apply(expression: ParseToken): String = expression {
    case Program(declarations) =>
      val strings = declarations.map(apply)
      strings.mkString("\n")

    case declaration: Declaration => declaration match {
      case DeclarationVar(identifier, expression) => s"(let $identifier ${apply(expression)})"
      case DeclarationStatement(statement) => apply(statement)
    }

    case statement: Statement => statement match {
      case StatementPrint(expression) => parenthesize("print", expression)
      case StatementExpression(expression) => apply(expression)
      case StatementIfElse(cond, statement1, statement2) => s"(if ${apply(cond)} ${apply(statement1)} ${apply(statement2)})"
      case StatementWhile(cond, statement) => s"(while ${apply(cond)} ${apply(statement)})"
      case StatementBlock(expressions) => s"{\n${expressions.map(apply).mkString("\n")}\n}"
    }

    case assign: Assignment => assign match {
      case AssignmentSet(identifier, expression) => s"(set $identifier ${apply(expression)})"
      case AssignmentLogicalOr(logicalOr) => apply(logicalOr)
    }

    case LogicalOr(logicalAnd, logicalAnds) =>
      val others = {logicalAnds.map(apply).mkString(" ")}
      s"(OR ${apply(logicalAnd)} ${others})"
    case LogicalAnd(equality, equalities) =>
      val others = equalities.map(apply).mkString(" ")
      s"(AND ${apply(equality)} $others)"
    case Equality(expression, expressions) => parenthesize(expression, expressions)
    case Comparison(expression, expressions) => parenthesize(expression, expressions)
    case Term(expression, expressions) => parenthesize(expression, expressions)
    case Factor(expression, expressions) => parenthesize(expression, expressions)
    case unary: Unary => unary match {
      case UnaryPrimary(primary) => apply(primary)
      case UnaryOperator(operator, primary) => s"${operator.token.lexeme}${apply(primary)}"
    }
    case primary: Primary => primary match {
      case NUMBER(int) => int.toString
      case STRING(s) => s
      case TRUE => ":t"
      case FALSE => ":f"
      case NIL => ":nil"
      case LiteralIdentifier(id) => s":${id.ident}"
      case Grouping(expression) =>
        f"G(${apply(expression)})"
    }
  }

  def parenthesize(name: String, exprs: Expression*) = {
    val subExprs: Seq[String] = exprs.map(apply)
    val exprStrings = subExprs.mkString(" ")
    f"($name $exprStrings)"
  }

  def parenthesize(expr: Expression, exprs: Seq[(Operator, Expression)]): String = {
    if (exprs.isEmpty) {
      apply(expr)
    } else {
      val exprStrings = exprs.map { case (operator, expression) => s"${operator.token.lexeme} ${apply(expression)}"}.mkString(" ")
      f"(${apply(expr)} $exprStrings)"
    }
  }
}
