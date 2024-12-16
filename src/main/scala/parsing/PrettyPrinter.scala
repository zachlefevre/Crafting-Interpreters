package com.lox.parsing

object PrettyPrinterSimpleParseTree {
  import ExpressionSimplified._

  def apply(expression: ParseToken): String = expression {
    case Program(statements) =>
      val strings = statements.map(apply)
      strings.mkString("\n")

    case statement: Statement => statement match {
      case StatementPrint(expression) => parenthesize("print", expression)
      case StatementExpression(expression) => apply(expression)
      case StatementVarDeclaration(identifier, expression) => s"(let $identifier ${apply(expression)})"

    }
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
    }
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
