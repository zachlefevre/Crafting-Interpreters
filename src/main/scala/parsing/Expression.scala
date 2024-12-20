package com.lox.parsing

import com.lox.lexer

case class Operator(token: lexer.Token)

object ParseToken {
  sealed trait ParseToken { self =>
    def apply[T](fn: ParseToken => T) = fn(self)

    def pretty = PrettyPrinterParseTree(self)

    def simplified = ParseToken.simplified(self)

  }

  case class Program(statements: List[Declaration]) extends ParseToken

  sealed trait Declaration extends ParseToken
  case class DeclarationVar(identifier: lexer.Token.IDENTIFIER, expression: StatementExpression) extends Declaration
  case class DeclarationStatement(statement: Statement) extends Declaration

  sealed trait Statement extends ParseToken
  case class StatementPrint(expression: Expression) extends Statement
  case class StatementExpression(expression: Expression) extends Statement

  sealed trait Expression extends ParseToken

  case class Equality(expression: Comparison, expressions: List[(Operator, Comparison)]) extends Expression
  case class Comparison(expression: Term, expressions: List[(Operator, Term)]) extends Expression
  case class Term(expression: Factor, expressions: List[(Operator, Factor)]) extends Expression
  case class Factor(expression: Unary, expressions: List[(Operator, Unary)]) extends Expression


  sealed trait Unary extends Expression
  case class UnaryPrimary(primary: Primary) extends Unary
  case class UnaryOperator(operator: Operator, unary: Unary) extends Unary
  sealed trait Primary extends Expression
  case class NUMBER(int: Double) extends Primary
  case class STRING(s: String) extends Primary
  case object TRUE extends Primary
  case object FALSE extends Primary
  case object NIL extends Primary
  case class LiteralIdentifier(id: lexer.Token.IDENTIFIER) extends Primary
  case class Grouping(expression: Expression) extends Primary

  def simplified(expression: ParseToken): ExpressionSimplified.Program = expression match {
    case Program(declarations) => ExpressionSimplified.Program(declarations.map(simplifiedDeclaration))
  }

  def simplifiedDeclaration(declaration: Declaration): ExpressionSimplified.Statement = declaration match {
    case DeclarationVar(identifier, StatementExpression(expression)) =>
      ExpressionSimplified.StatementVarDeclaration(identifier, ExpressionSimplified.StatementExpression(simplifiedExpression(expression)))
    case DeclarationStatement(statement) => simplifiedStatement(statement)
  }

  def simplifiedStatementExpression(statement: Expression): ExpressionSimplified.StatementExpression =
    ExpressionSimplified.StatementExpression(simplifiedExpression(statement))

  def simplifiedStatement(expression: Statement): ExpressionSimplified.Statement = expression match {
    case StatementExpression(expression) => simplifiedStatementExpression(expression)
    case StatementPrint(expression) => ExpressionSimplified.StatementPrint(simplifiedExpression(expression))
  }

  def simplifiedExpression(left: Expression, expressions: List[(Operator, Expression)]): ExpressionSimplified.Expression = expressions match {
      case Nil => simplifiedExpression(left)
      case (operator, right) :: rest => ExpressionSimplified.Binary(simplifiedExpression(left), operator, simplifiedExpression(right, rest))
    }

  def simplifiedExpression(expression: Expression): ExpressionSimplified.Expression = expression match {
    case Equality(expression, expressions) => simplifiedExpression(expression, expressions)
    case Comparison(expression, expressions) => simplifiedExpression(expression, expressions)
    case Term(expression, expressions) => simplifiedExpression(expression, expressions)
    case Factor(expression, expressions) => simplifiedExpression(expression, expressions)
      /*
    case assignment: Assignment => assignment match {
      case AssignmentSet(id, StatementExpression(expression)) => ExpressionSimplified.Assignment(id, simplifiedStatementExpression(expression))
      case AssignmentEquality(equality) => simplifiedExpression(equality)
    }
       */
    case unary: Unary => unary match {
      case UnaryPrimary(primary) => simplifiedExpression(primary)
      case UnaryOperator(operator, primary) => ExpressionSimplified.Unary(operator, simplifiedExpression(primary))
    }
    case primary: Primary => primary match {
      case NUMBER(int) => ExpressionSimplified.NUMBER(int)
      case STRING(s) => ExpressionSimplified.STRING(s)
      case TRUE => ExpressionSimplified.TRUE
      case FALSE => ExpressionSimplified.FALSE
      case NIL => ExpressionSimplified.NIL
      case LiteralIdentifier(id) => ExpressionSimplified.LiteralIdentifier(id)
      case Grouping(expression) =>
        ExpressionSimplified.Grouping(simplifiedExpression(expression))
    }
  }
}

object ExpressionSimplified {

  sealed trait ParseToken { self =>
    def apply[T](fn: ParseToken => T) = fn(self)

    def pretty = PrettyPrinterSimpleParseTree(self)
  }

  case class Program(statements: List[Statement]) extends ParseToken

  sealed trait Statement extends ParseToken
  case class StatementPrint(expression: Expression) extends Statement
  case class StatementExpression(expression: Expression) extends Statement
  case class StatementVarDeclaration(identifier: lexer.Token.IDENTIFIER, expression: StatementExpression) extends Statement

  sealed trait Expression extends ParseToken
  case class Assignment(identifier: lexer.Token.IDENTIFIER, expression: StatementExpression) extends Expression

  case class Binary(left: Expression, operator: Operator, right: Expression) extends Expression
  case class Grouping(expression: Expression) extends Expression

  sealed trait Literal extends Expression
  case class NUMBER(int: Double) extends Literal
  case class STRING(s: String) extends Literal
  case object TRUE extends Literal
  case object FALSE extends Literal
  case object NIL extends Literal
  case class LiteralIdentifier(id: lexer.Token.IDENTIFIER) extends Literal
  case class Unary(operator: Operator, expression: Expression) extends Expression
}
