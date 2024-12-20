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

  case class Environment(state: Map[Token.IDENTIFIER, Object]) {
    def success(ob: Object) = Success(ob, this)
    def add(key: Token.IDENTIFIER, value: Object) = Environment(state + (key -> value))
    def apply(key: Token.IDENTIFIER) = state.get(key)
  }
  object Environment {
    def empty = Environment(Map.empty)
  }


  sealed trait Execution { self =>
    def map(fn: Object => Object): Execution
    def tap(fn: Object => Unit): Execution = self.map { ob =>
      fn(ob)
      self
    }
    def flatMap(fn: Object => Execution): Execution
  }
  case class Success(ob: Object, environment: Environment) extends Execution {
    override def map(fn: Object => Object): Execution = Success(fn(ob), environment)
    def flatMap(fn: Object => Execution): Execution = fn(ob)
  }
  case class Failure(err: RError) extends Execution {
    override def map(fn: Object => Object): Execution = this
    def flatMap(fn: Object => Execution): Execution = this
  }

  case class TypeError(expected: String, got: Object) extends RError(s"Expected $expected but got $got")
  case class BinaryError(operator: Operator, left: Object, right: Object, expectedLeft: String, expectedRight: String) extends RError(s"Expected ${operator.token.lexeme}($expectedLeft, $expectedRight) but received ${operator.token.lexeme}($left, $right)")
  case class Multiple(left: RError, right: RError) extends RError(s"${left.message} or\n${right.message}")


  def interpret(program: Program): Execution = {
    program.statements.foldLeft[Execution](Success(null.asInstanceOf[Object], Environment.empty)){ (execution, statement) =>
      execution match {
        case fail @ Failure(err) => fail
        case Success(ob, environment) =>
          interpretStatement(statement, environment)
      }
  }}

  def interpretStatement(expression: Statement, environment: Environment): Execution = expression match {
    case StatementPrint(expression) =>
      interpretExpression(expression, environment)
        .tap(value => println(s"${Console.GREEN}~> $value${Console.RESET}"))

    case StatementExpression(expression) =>
      interpretExpression(expression, environment)

    case StatementVarDeclaration(id, expr) =>
      interpretStatement(expr, environment) match {
        case Success(ob, env) =>
          val newEnv = env.add(id, ob)
          Success(ob, newEnv)
        case fail @ Failure(_) => fail
      }
  }

  def interpretExpression(expression: Expression, environment: Environment): Execution =
    expression.apply[Execution] {
      case l: Literal => Success(l match {
        case STRING(s) => s
        case NUMBER(n) => n.asInstanceOf[Object]
        case TRUE => true.asInstanceOf[Object]
        case FALSE => false.asInstanceOf[Object]
        case NIL => null
        case LiteralIdentifier(id) => environment(id).get
      }, environment)
      case Grouping(expr) => interpretExpression(expr, environment)
      case Unary(operator, expression) => operator match {
        case Operator(Token.MINUS(_)) => interpretExpression(expression, environment).flatMap {
          case expression if expression.isInstanceOf[Double] => environment.success(operate[Double, Double](- _)(expression))
          case expression => Failure(TypeError("Double", expression))
        }
        case Operator(Token.BANG(_)) => interpretExpression(expression, environment).map(expr => (!truthy(expr)).asInstanceOf[Object])
      }
      case Binary(left, operator, right) => operator match {
        case Operator(Token.MINUS(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   environment.success(operate[Double, Double, Double](_ - _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.PLUS(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[String] && r.isInstanceOf[String] =>   environment.success(operate[String, String, String](_ + _)(l, r))
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   environment.success(operate[Double, Double, Double](_ + _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double").or(BinaryError(operator, l, r, "String", "String")))
          }
        } yield r
        case Operator(Token.SLASH(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] && r == 0 =>   Failure(BinaryError(operator, l, r, "Double", "Double - {0}"))
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => environment.success(operate[Double, Double, Double](_ / _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.STAR(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] =>   environment.success(operate[Double, Double, Double](_ * _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.GREATER(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => environment.success(operate[Double, Double, Boolean](_ > _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.GREATER_EQUAL(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => environment.success(operate[Double, Double, Boolean](_ >= _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.LESS(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => environment.success(operate[Double, Double, Boolean](_ < _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r
        case Operator(Token.LESS_EQUAL(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
          r <- (l, r) match {
            case (l, r) if l.isInstanceOf[Double] && r.isInstanceOf[Double] => environment.success(operate[Double, Double, Boolean](_ <= _)(l, r))
            case (l, r) => Failure(BinaryError(operator, l, r, "Double", "Double"))
          }
        } yield r

        case Operator(Token.BANG_EQUAL(_)) => for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
        } yield (!isEqual(left, right)).asInstanceOf[Object]

        case Operator(Token.EQUAL_EQUAL(_)) =>for {
          l <- interpretExpression(left, environment)
          r <- interpretExpression(right, environment)
        } yield (isEqual(left, right)).asInstanceOf[Object]
      }
    }
}
