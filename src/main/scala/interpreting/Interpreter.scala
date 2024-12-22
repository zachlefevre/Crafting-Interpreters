package com.lox.interpreting

import scala.reflect._

import com.lox.parsing.{ExpressionSimplified, Operator}
import com.lox.parsing.ExpressionSimplified._
import com.lox.lexer.Token

object Interpreter {

  case class ZoxCallable(name: String)(arity: Int, fn: (Environment, List[Object]) => Execution) {
    def apply(environment: Environment, obs: List[Object]) = {
      if (obs.length == arity) {
        fn(environment, obs)
      } else {
        Failure(FunctionWrongArity(name, arity, obs.length))
      }
    }
  }

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

  trait Environment { self =>
    val state: Map[Token, Object]

    def success(ob: Object) = Success(ob, this)
    def add(key: Token, value: Object): Environment
    def update(key: Token, value: Object): Option[Environment]
    def apply(key: Token): Option[Object]
    def contains(key: Token): Boolean

    def extend: Environment
    def pop: Environment
  }

  case class Base(state: Map[Token, Object]) extends Environment {
    override def add(key: Token, value: Object) = Base(state + (key -> value))
    def update(key: Token, value: Object): Option[Environment] = {
      if (contains(key)) {
        Some(add(key, value))
      } else None
    }
    def apply(key: Token): Option[Object] = state.get(key)
    def contains(key: Token): Boolean = state.contains(key)
    def extend: Environment = Frame(Map.empty, this)
    def pop: Environment = ???
  }


  case class Frame(state: Map[Token, Object], parent: Environment) extends Environment {
    def add(key: Token, value: Object): Environment = Frame(state + (key -> value), parent)
    def update(key: Token, value: Object): Option[Environment] = {
      if (state.contains(key)) {
        Some(Frame(state + (key -> value), parent))
      } else parent.update(key, value).map(Frame(state, _))
    }
    def apply(key: Token): Option[Object] = state.get(key) orElse parent(key)
    def contains(key: Token): Boolean = state.contains(key) || parent.contains(key)
    def extend: Environment = Frame(Map.empty, this)
    def pop: Environment = parent
  }


  object Environment {
    def empty = Base(Map(
      Token.IDENTIFIER("clock", -1) -> ZoxCallable("clock")(0, (environment, _) => environment.success((System.currentTimeMillis.toDouble).asInstanceOf[Object])).asInstanceOf[Object]
    ))
  }


  sealed trait Execution { self =>
    def map(fn: Object => Object): Execution
    def tap(fn: Object => Unit): Execution = self.map { ob =>
      fn(ob)
      ob
    }
    def flatMap(fn: Object => Execution): Execution
  }
  case class Success(ob: Object, environment: Environment) extends Execution {
    override def map(fn: Object => Object): Execution = Success(fn(ob), environment)
    def flatMap(fn: Object => Execution): Execution = {
      fn(ob)
    }
  }
  case class Failure(err: RError) extends Execution {
    override def map(fn: Object => Object): Execution = this
    def flatMap(fn: Object => Execution): Execution = this
  }

  case class TypeError(expected: String, got: Object) extends RError(s"Expected $expected but got $got")
  case class BinaryError(operator: Operator, left: Object, right: Object, expectedLeft: String, expectedRight: String) extends RError(s"Expected ${operator.token.lexeme}($expectedLeft, $expectedRight) but received ${operator.token.lexeme}($left, $right)")
  case class Multiple(left: RError, right: RError) extends RError(s"${left.message} or\n${right.message}")

  case class VariableUndefined(variable: Token) extends RError(s"Variable $variable was undefined")
  case class InvokingNonFunction(ob: Object) extends RError(s"Object $ob is not a function")
  case class FunctionWrongArity(ob: Object, realArity: Int, mistakenArity: Int) extends RError(s"Function $ob has arity ${realArity} but was invoked with ${mistakenArity} arguments")


  def interpret(program: Program): Execution = {
    program.statements.foldLeft[Execution](Success(null.asInstanceOf[Object], Environment.empty)){ (execution, statement) =>
      execution match {
        case fail @ Failure(err) => fail
        case Success(ob, environment) =>
          interpretStatement(statement, environment)
      }
    }}

  def interpretStatement(expression: Statement, environment: Environment): Execution = expression match {
    case Statement.Print(expression) =>
      interpretExpression(expression, environment)
        .tap(value => println(s"${Console.GREEN}~> $value${Console.RESET}"))

    case Statement.SExpression(expression) =>
      interpretExpression(expression, environment)

    case Statement.Var(id, expr) =>
      interpretStatement(expr, environment) match {
        case Success(ob, env) =>
          val newEnv = env.add(id, ob)
          Success(ob, newEnv)
        case fail @ Failure(_) => fail
      }

    case Statement.While(cond, stmt) => {
      interpretExpression(cond, environment) match {
        case Success(ob, environment) if truthy(ob) => interpretStatement(stmt, environment) match {
          case Success(ob, environment) => interpretStatement(expression, environment)
          case other => other
          case Success(ob, environment) => environment.success(null.asInstanceOf[Object])
        }
        case other => other
      }
    }

    case Statement.IfElse(cond, stmt1, stmt2) =>
      interpretExpression(cond, environment) match {
        case Success(ob, environment) =>
          if (truthy(ob)) {
            interpretStatement(stmt1, environment)
          } else interpretStatement(stmt2, environment)
        case fail @ Failure(_) => fail
      }

    case Statement.Block(expressions) =>
      val newEnvironment = environment.extend
      expressions.foldLeft[Execution](newEnvironment.success(().asInstanceOf[Object])) { case (previous, expression) =>
        previous match {
          case Success(value, env) => interpretStatement(expression, env)
          case fail => fail
        }
      } match {
        case Success(value, env) => Success(value, env.pop)
        case fail => fail
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
      }, environment)
      case Assignment(id, expression) =>
        interpretExpression(expression, environment) match {
          case Success(ob, newEnv) =>
            environment.update(id, ob) match {
              case Some(env) =>
                Success(ob, env)
              case None =>
                Failure(VariableUndefined(id))
            }
          case fail @ Failure(_) => fail
        }


      case Func(params, body) =>
        environment.success(ZoxCallable("λ")(params.length, (environment, args) => {
          val newEnvironment = params.zip(args).foldLeft(environment.extend) { case (environment, (param, arg)) => environment.add(param, arg)}
          interpretStatement(body, newEnvironment) match {
            case Success(ob, env) => Success(ob, env.pop)
            case other => other
          }
        }))

      case Call(callee, arguments) =>
        interpretExpression(callee, environment) match {
          case Success(callee, environment) if callee.isInstanceOf[ZoxCallable] =>
            val callable = callee.asInstanceOf[ZoxCallable]
            val (executionArgs, env) = arguments.foldLeft[(List[Execution], Environment)](List.empty -> environment) { case ((list, environment), argument) =>
              interpretExpression(argument, environment) match {
                case succ @ Success(_, environment) => (succ :: list) -> environment
                case fail @ Failure(_) => (fail :: list) -> environment
              }
            }
            executionArgs.collectFirst {
              case fail @ Failure(_) => fail
            } match {
              case Some(fail) => return fail
              case None =>
            }
            val args = executionArgs.reverse.map {
              case Success(ob, _) => ob
            }

            callable(env, args)
          case Success(other, environment) =>
            Failure(InvokingNonFunction(other))

          case other => other
        }

      case Variable(id) => environment(id).map(environment.success).getOrElse(Failure(VariableUndefined(id)))
      case Grouping(expr) => interpretExpression(expr, environment)
      case Unary(operator, expression) => operator match {
        case Operator(Token.MINUS(_)) => interpretExpression(expression, environment).flatMap {
          case expression if expression.isInstanceOf[Double] => environment.success(operate[Double, Double](- _)(expression))
          case expression => Failure(TypeError("Double", expression))
        }
        case Operator(Token.BANG(_)) => interpretExpression(expression, environment).map(expr => (!truthy(expr)).asInstanceOf[Object])
      }
      case Logical(left, operator, right) => operator match {
        case Operator(Token.AND(_)) =>
          interpretExpression(left, environment) match {
            case Success(ob, environment) if truthy(ob) => interpretExpression(right, environment)
            case success @ Success(_, environment) => success
            case other => other
          }
        case Operator(Token.OR(_)) =>
          interpretExpression(left, environment) match {
            case success @ Success(ob, environment) if truthy(ob) => success
            case Success(_, environment)  => interpretExpression(right, environment)
            case other => other
          }
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
