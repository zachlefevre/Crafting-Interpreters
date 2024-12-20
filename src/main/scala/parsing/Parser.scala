package com.lox.parsing

import com.lox.lexer

case class State(current: Int) {
  def advance = this.copy(current = current + 1)
}

object State {
  def default = State(0)
}



case class Parser(tokens: List[lexer.Token]) {
  def tokenAt(state: State) = tokens.lift(state.current)

  def matchAtN[A](state: State, n: Int)(fn: PartialFunction[(List[lexer.Token], State, State), Option[A]]): Option[A] = {
    val (tokens, (newState, _)) = Util.unfold[Option[lexer.Token], (State, Int)](state -> 0) { case (state, length) =>
      if (length >= n) None else {
        Some(tokenAt(state) -> (state.advance, length + 1))
      }
    }

    fn.lift((tokens.flatten, state, newState)).flatten
  }

  def primary(state: State): (ParseToken.Unary, State) = {
    val (expr, newState) = tokenAt(state) match {
      case Some(lexer.Token.TRUE(_)) => ParseToken.TRUE -> state.advance
      case Some(lexer.Token.FALSE(_)) => ParseToken.FALSE -> state.advance
      case Some(lexer.Token.NIL(_)) => ParseToken.NIL -> state.advance

      case Some(lexer.Token.NUMBER(n, _)) => ParseToken.NUMBER(n) -> state.advance
      case Some(lexer.Token.STRING(n, _)) => ParseToken.STRING(n) -> state.advance
      case Some(id @ lexer.Token.IDENTIFIER(_, _)) => ParseToken.LiteralIdentifier(id) -> state.advance
      case Some(lexer.Token.LEFT_PAREN(_)) => expression(state.advance) match {
        case (expr, state) => tokenAt(state) match {
          case Some(lexer.Token.RIGHT_PAREN(_)) => ParseToken.Grouping(expr) -> state.advance
        }
      }
    }

    ParseToken.UnaryPrimary(expr) -> newState
  }

  def unary(state: State): (ParseToken.Unary, State) = {
    tokenAt(state) match {
      case Some(op @ lexer.Token.BANG(_)) => unary(state.advance) match { case (expression, state) =>
        ParseToken.UnaryOperator(Operator(op), expression) -> state
      }
      case Some(op @ lexer.Token.MINUS(_)) => unary(state.advance) match { case (expression, state) =>
        ParseToken.UnaryOperator(Operator(op), expression) -> state
      }
      case _ => primary(state)
    }
  }

  def factor(state: State): (ParseToken.Factor, State) = {
    unary(state) match {
      case (expr, state) =>
        val (expressions, newState) = Util.unfold[(Operator, ParseToken.Unary), State](state) { state =>
          tokenAt(state) match {
            case Some(op @ lexer.Token.SLASH(_)) => unary(state.advance) match {
              case (expr, state) =>
                Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.STAR(_)) => unary(state.advance) match {
              case (expr, state) =>
                Some((Operator(op) -> expr, state))
            }
            case _ => None
          }
        }
        ParseToken.Factor(expr, expressions) -> newState
    }
  }

  def term(state: State): (ParseToken.Term, State) = {
    factor(state) match {
      case (expr, state) =>
        val (expressions, newState) = Util.unfold[(Operator, ParseToken.Factor), State](state) { state =>
          tokenAt(state) match {
            case Some(op @ lexer.Token.MINUS(_)) => factor(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.PLUS(_)) => factor(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case _ => None
          }
        }
        ParseToken.Term(expr, expressions) -> newState
    }
  }

  def comparison(state: State): (ParseToken.Comparison, State) = {
    term(state) match {
      case (expr, state) =>
        val (expressions, newState) = Util.unfold[(Operator, ParseToken.Term), State](state) { state =>
          tokenAt(state) match {
            case Some(op @ lexer.Token.GREATER(_)) => term(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.GREATER_EQUAL(_)) => term(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.LESS(_)) => term(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.LESS_EQUAL(_)) => term(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }

            case _ => None
          }
        }
        ParseToken.Comparison(expr, expressions) -> newState
    }
  }


  def equality(state: State): (ParseToken.Equality, State) = {
    comparison(state) match { case (expr, state) =>
        val (expressions, newState) = Util.unfold[(Operator, ParseToken.Comparison), State](state) { state =>
          tokenAt(state) match {
            case Some(op @ lexer.Token.BANG_EQUAL(_)) => comparison(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case Some(op @ lexer.Token.EQUAL_EQUAL(_)) => comparison(state.advance) match {
              case (expr, state) => Some((Operator(op) -> expr, state))
            }
            case _ => None
          }
        }
        ParseToken.Equality(expr, expressions) -> newState
    }
  }

  def expression(state: State): (ParseToken.Expression, State) = equality(state)

  def printStatement(state: State): (ParseToken.StatementPrint, State) = {
    val (toPrint, newState) = expression(state)
    tokenAt(newState) match {
      case Some(lexer.Token.SEMICOLON(_)) => (ParseToken.StatementPrint(toPrint) -> newState.advance)
      case _ => ???
    }
  }

  def expressionStatement(state: State): (ParseToken.StatementExpression, State) = {
    val (expr, newState) = expression(state)
    tokenAt(newState) match {
      case Some(lexer.Token.SEMICOLON(_)) => (ParseToken.StatementExpression(expr) -> newState.advance)
      case _ => ???
    }
  }

  def statement(state: State): (ParseToken.Statement, State) = {
    tokenAt(state) match {
      case Some(lexer.Token.PRINT(_)) => printStatement(state.advance)
      case _ => expressionStatement(state)
    }
  }

  def varDeclaration(state: State): (ParseToken.Declaration, State) = {
    tokenAt(state) match {
      case Some(id @ lexer.Token.IDENTIFIER(_, _)) => tokenAt(state.advance) match {
        case Some(lexer.Token.EQUAL(_)) =>
          val (statementExpr, newState) = expressionStatement(state.advance.advance)
          ParseToken.DeclarationVar(id, statementExpr) -> newState
      }
      case _ => ???
    }
  }

  def declaration(state: State): (ParseToken.Declaration, State) = {
    tokenAt(state) match {
      case Some(lexer.Token.VAR(_)) => varDeclaration(state.advance)
      case _ => statement(state) match {
        case (statement, state) => ParseToken.DeclarationStatement(statement) -> state
      }
    }
  }

  def program(state: State): (ParseToken.Program, State) = {

    val (declarations, newState) = Util.unfold[ParseToken.Declaration, State](state) { state =>
      matchAtN(state, 1) {
        case (List(lexer.Token.EOF(_)), _, _) => None
        case (_, state, _) => Some(declaration(state))
      }
    }
    ParseToken.Program(declarations) -> newState
  }

}


object Util {
  def unfold[A, S](state: S)(fn: S => Option[(A, S)]): (List[A], S) = {
    val list = List.unfold[(A, S), S](state) {s =>
      fn(s).map { case (a, s) => ((a, s), s)}
    }

    val lastState: S = list.foldLeft(state) { case (_, (_, last)) => last }

    list.map { case (value, _) => value} -> lastState
  }
}
