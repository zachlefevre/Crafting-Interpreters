package com.lox.lexer


class TokenizationError(line: Int, where: String, message: String) extends Exception(s"[line $line] Error ${where}: $message")
case class UnexpectedToken(line: Int, where: String, char: Char) extends TokenizationError(line, where, s"Unexpected character $char")

// we could use vars for these values, or (as I've chosen) use a state transformer
case class State(current: Int, line: Int) {
  def advance = this.copy(current = current + 1)
  def newline(n: Int = 1) = this.copy(line = line + n)
}

case class Scanner(source: String) {

  def endOfInput(state: State): Boolean = state.current >= source.length

  def tokenAt(state: State): Option[Char] =
    if (endOfInput(state)) None else Some(source(state.current))

  def scanToken(state: State): Either[TokenizationError, (State, Token)] = {
    if (endOfInput(state)) Right(state.advance -> Token.EOF(state.line)) else {
      val State(current, line) = state
      source(current) match {
        case  '(' => Right(state.advance -> Token.LEFT_PAREN(line))
        case  ')' => Right(state.advance -> Token.RIGHT_PAREN(line))

        case  '{' => Right(state.advance -> Token.LEFT_BRACE(line))
        case  '}' => Right(state.advance -> Token.RIGHT_BRACE(line))

        case  ',' => Right(state.advance -> Token.COMMA(line))
        case  '.' => Right(state.advance -> Token.DOT(line))
        case  '-' => Right(state.advance -> Token.MINUS(line))
        case  '+' => Right(state.advance -> Token.PLUS(line))

        case  ';' => Right(state.advance -> Token.SEMICOLON(line))
        case  '*' => Right(state.advance -> Token.STAR(line))

        case  '!' if tokenAt(state.advance) == Some('=') => Right(state.advance.advance -> Token.BANG_EQUAL(line))
        case  '!' => Right(state.advance -> Token.BANG(line))

        case  '=' if tokenAt(state.advance) == Some('=') => Right(state.advance.advance -> Token.EQUAL_EQUAL(line))
        case  '=' => Right(state.advance -> Token.EQUAL(line))

        case  '<' if tokenAt(state.advance) == Some('=') => Right(state.advance.advance -> Token.LESS_EQUAL(line))
        case  '<' => Right(state.advance -> Token.LESS(line))

        case  '>' if tokenAt(state.advance) == Some('=') => Right(state.advance.advance -> Token.GREATER_EQUAL(line))
        case  '>' => Right(state.advance -> Token.GREATER(line))

        case '/' if tokenAt(state.advance) == Some('/') =>
          val (newState, _, _) = moveWhile(_ != '\n')(state.advance.advance)
          val stateWithLines = newState
            .advance.newline()

          scanToken(stateWithLines)

        case  '/' => Right(state.advance -> Token.SLASH(line))

        case '\n'  =>
          scanToken(state.advance.newline())

        case c if c.isWhitespace =>
          scanToken(state.advance)

        case '"' =>
          moveWhile(_ != '"')(state.advance) match {
            case (state, passed, false) => Right(state.advance -> Token.STRING(passed.mkString, line))
            case _ => Left(new TokenizationError(line, "", "String never ends"))
          }

        case n if n.isDigit =>
          moveWhile(char => char == '.' || char.isDigit)(state) match {
            case (state, passed, _) if passed.filter(_ == '.').length > 1 => Left(new TokenizationError(line, "", "number literal contains more than one '.'"))
            case (state, passed, _) => Right(state -> Token.NUMBER(passed.mkString.toDouble, line))
          }

        case c if c.isLetter =>
          moveWhile(char => char.isLetterOrDigit)(state) match {
            case (state, passed, _) => {
              val token = passed.mkString match {
                case "and" => Token.AND(line)
                case "or" => Token.OR(line)
                case "class" => Token.CLASS(line)
                case "else" => Token.ELSE(line)
                case "false" => Token.FALSE(line)
                case "fun" => Token.FUN(line)
                case "for" => Token.FOR(line)
                  case "if" => Token.IF(line)
                    case "nil" => Token.NIL(line)
                case "print" => Token.PRINT(line)
                case "return" => Token.RETURN(line)
                case "super" => Token.SUPER(line)
                case "this" => Token.THIS(line)
                case "true" => Token.TRUE(line)
                case "var" => Token.VAR(line)
                case "while" => Token.WHILE(line)
                  case other => Token.IDENTIFIER(other, line)
              }
              Right(state -> token)

            }

          }

        case other => Left(UnexpectedToken(line, "", other))
      }
    }
  }

  def moveWhile(pred: Char => Boolean)(state: State): (State, List[Char], Boolean) = {
    def go(pred: Char => Boolean)(state: State, passed: List[Char]): (State, List[Char], Boolean) = {
      tokenAt(state) match {
        case Some(c) if pred(c) => go(pred)(state.advance, c :: passed)
        case Some(_) => (state, passed.reverse, false)
        case None => (state, passed.reverse, true)
      }
    }
    go(pred)(state, List.empty)
  }

  def scanTokens: Either[TokenizationError, List[Token]] = {
    val tokens: List[Either[TokenizationError, Token]] = List.unfold[Either[TokenizationError, Token], (State, Boolean)](State(0, 0) -> false) { case (state, done) =>
      if(done) None else scanToken(state) match {
        case Left(error) => Some(Left(error) -> (state, true))
        case Right((state, token @ Token.EOF(_))) => Some(Right(token) -> (state, true))
        case Right((state, token)) => Some(Right(token) -> (state, false))
      }
    }

    Util.sequence(tokens).right.map(_.reverse)
  }
}


object Util {
  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {
    l.foldLeft[Either[E, List[A]]](Right(List.empty)) {
      case (state: Either[E, List[A]], next: Either[E, A]) =>
        for {
          items <- state
          value <- next
        } yield value :: items
    }
  }
}
