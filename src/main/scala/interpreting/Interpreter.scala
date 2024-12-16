package com.lox.interpreting

import scala.reflect._

import com.lox.parsing.{ExpressionSimplified, Operator}
import com.lox.parsing.ExpressionSimplified._
import com.lox.lexer.Token


object Interpreter {

  def operate[A, B](fn: (A) => B)(a: Object) =
    fn(a.asInstanceOf[A]).asInstanceOf[Object]

  def operate[A, B, C](fn: (A, B) => C)(a: Object, b: Object) =
    fn(a.asInstanceOf[A], b.asInstanceOf[B]).asInstanceOf[Object]

  def truthy(obj: Object): Boolean = {
    if (obj == null) false
    else if (obj.isInstanceOf[Boolean]) obj.asInstanceOf[Boolean]
    else true
  }

  def isEqual(left: Object, right: Object) =
    (Option(left), Option(right)) match {
      case (None, None) => true
      case (None, Some(_)) => false
      case (Some(_), None) => false
      case (Some(v0), Some(v1)) => v0.equals(v1)
    }

  abstract class RError(val message: String) extends Exception(message) {
    def or(other: RError) = Multiple(this, other)
  }
  case class TypeError(expected: String, got: Object) extends RError(s"Expected $expected but got $got")
  case class BinaryError(operator: Operator, left: Object, right: Object, expectedLeft: String, expectedRight: String) extends RError(s"Expected ${operator.token.lexeme}($expectedLeft, $expectedRight) but received ${operator.token.lexeme}($left, $right)")
  case class Multiple(left: RError, right: RError) extends RError(s"${left.message} or\n${right.message}")


  def interpret(program: Program): Either[RError, Object] = {
    program.statements.foreach(statement => interpretStatement(statement) match {
      case Left(err) => return Left(err)
      case n =>
    })
    Right(().asInstanceOf[Object])
  }

  def interpretStatement(expression: Statement): Either[RError, Object] = expression match {
    case StatementPrint(expression) =>
      interpretExpression(expression)
        .map(value => println(s"${Console.GREEN}~> $value${Console.RESET}"))
        .flatMap(_ => Right(().asInstanceOf[Object]))

    case StatementExpression(expression) =>
      interpretExpression(expression)
        .flatMap(_ => Right(().asInstanceOf[Object]))

    case StatementVarDeclaration(id, expr) => interpretStatement(expr)

  }

  def interpretExpression(expression: Expression): Either[RError, Object] =
    expression.apply[Either[RError, Object]] {
      case l: Literal => Right(l match {
        case STRING(s) => s
        case NUMBER(n) => n.asInstanceOf[Object]
        case TRUE => true.asInstanceOf[Object]
        case FALSE => false.asInstanceOf[Object]
        case NIL => null
      })
      case Grouping(expr) => interpretExpression(expr)
      case Unary(operator, expression) => operator match {
        case Operator(Token.MINUS(_)) => interpretExpression(expression).flatMap {
          case expression if expression.isInstanceOf[Double] => Right(operate[Double, Double](- _)(expression))
          case expression => Left(TypeError("Double", expression))
        }
        case Operator(Token.BANG(_)) => interpretExpression(expression).map(expr => !truthy(expr)).map(_.asInstanceOf[Object])
      }
      case Binary(left, operator, right) => operator match {
        case Operator(Token.MINUS(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   Right(operate[Double, Double, Double](_ - _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.PLUS(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[String] && r.isInstanceOf[String] =>   Right(operate[String, String, String](_ + _)(l, r))
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   Right(operate[Double, Double, Double](_ + _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double").or(BinaryError(operator, l, r, "String", "String")))
          }
        } yield r
        case Operator(Token.SLASH(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] && r == 0 =>   Left(BinaryError(operator, l, r, "Double", "Double - {0}"))
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => Right(operate[Double, Double, Double](_ / _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.STAR(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   Right(operate[Double, Double, Double](_ * _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.GREATER(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => Right(operate[Double, Double, Boolean](_ > _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.GREATER_EQUAL(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => Right(operate[Double, Double, Boolean](_ >= _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.LESS(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => Right(operate[Double, Double, Boolean](_ < _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.LESS_EQUAL(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => Right(operate[Double, Double, Boolean](_ <= _)(l, r))
            case (l, r) => Left(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.BANG_EQUAL(_)) => for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
        } yield (!isEqual(left, right)).asInstanceOf[Object]

        case Operator(Token.EQUAL_EQUAL(_)) =>for {
          l <- interpretExpression(left)
          r <- interpretExpression(right)
        } yield (isEqual(left, right)).asInstanceOf[Object]
      }
    }
}
