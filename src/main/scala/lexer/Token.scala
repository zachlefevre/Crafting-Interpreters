package com.lox.lexer

sealed trait Token {
  val line: Int
  val lexeme: String
}
object Token {
  case class LEFT_PAREN(line: Int) extends Token { val lexeme = "("}
  case class RIGHT_PAREN(line: Int) extends Token { val lexeme = ")"}
  case class LEFT_BRACE(line: Int) extends Token { val lexeme = "{"}
  case class RIGHT_BRACE(line: Int) extends Token { val lexeme = "}"}

  case class COMMA(line: Int) extends Token { val lexeme = ","}
  case class DOT(line: Int) extends Token { val lexeme = "."}
  case class MINUS(line: Int) extends Token { val lexeme = "-"}
  case class PLUS(line: Int) extends Token { val lexeme = "+"}
  case class SEMICOLON(line: Int) extends Token { val lexeme = ";"}
  case class SLASH(line: Int) extends Token { val lexeme = "/"}
  case class STAR(line: Int) extends Token { val lexeme = "*"}

  case class BANG(line: Int) extends Token { val lexeme = "!"}
  case class BANG_EQUAL(line: Int) extends Token { val lexeme = "!="}
  case class EQUAL(line: Int) extends Token { val lexeme = "="}
  case class EQUAL_EQUAL(line: Int) extends Token { val lexeme = "=="}
  case class GREATER(line: Int) extends Token { val lexeme = ">"}
  case class GREATER_EQUAL(line: Int) extends Token { val lexeme = ">="}
  case class LESS(line: Int) extends Token { val lexeme = "<"}
  case class LESS_EQUAL(line: Int) extends Token { val lexeme = "<="}

  case class IDENTIFIER(ident: scala.Predef.String, line: Int) extends Token { val lexeme = ident}
  case class STRING(string: scala.Predef.String, line: Int) extends Token { val lexeme = string}
  case class NUMBER(num: Double, line: Int) extends Token { val lexeme = num.toString}

  case class AND(line: Int) extends Token { val lexeme = "and"}
  case class CLASS(line: Int) extends Token { val lexeme = "class"}
  case class ELSE(line: Int) extends Token { val lexeme = "else"}
  case class FALSE(line: Int) extends Token { val lexeme = "#f"}
  case class FUN(line: Int) extends Token { val lexeme = "λ"}
  case class FOR(line: Int) extends Token { val lexeme = "for"}
  case class IF(line: Int) extends Token { val lexeme = "if"}
  case class NIL(line: Int) extends Token { val lexeme = "nil"}
  case class OR(line: Int) extends Token { val lexeme = "or"}
  case class PRINT(line: Int) extends Token { val lexeme = "print"}
  case class RETURN(line: Int) extends Token { val lexeme = "->>"}
  case class SUPER(line: Int) extends Token { val lexeme = "super"}
  case class THIS(line: Int) extends Token { val lexeme = "this"}
  case class TRUE(line: Int) extends Token { val lexeme = "#t"}
  case class VAR(line: Int) extends Token { val lexeme = "var"}
  case class WHILE(line: Int) extends Token { val lexeme = "while"}
  case class EOF(line: Int) extends Token { val lexeme = "EOF"}
}
