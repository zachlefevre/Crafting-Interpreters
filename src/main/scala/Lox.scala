package com.lox


import java.nio.file._
import java.nio.charset._
import java.io.{BufferedReader, InputStreamReader}

object Lox extends App {
  args.toList match {
    case Nil => runPrompt
    case head :: Nil => runFile(head)
    case _ =>
      println("Usage: jlox [script]")
      sys.exit(64)
  }

  def runPrompt: Unit = {
    val reader = new BufferedReader(new InputStreamReader(System.in))
    while (true) {
      print("> ")
      val line = Option(reader.readLine())
      line match {
        case Some(line) => run(line)
        case None =>
          println("May your days be aimless")
          return
      }
    }
  }

  def runFile(path: String) = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))
  }

  def run(source: String) = {
    val scanner = lexer.Scanner(source)
    scanner.scanTokens match {
      case Left(value) => reportError(value)
      case Right(tokens) =>
        println(s"tokens:\t\t\t${tokens}\n")

        val parser = parsing.Parser(tokens)
        val (ast, _) = parser.program(parsing.State.default)
        println(s"pretty ast:\t\t${ast.pretty}\n")
        println(s"pretty simplified ast:\t${ast.simplified.pretty}\n")

        val interpreted = interpreting.Interpreter.interpret(ast.simplified) match {
          case Right(value) => s"${Console.GREEN}It Worked!\n$value${Console.RESET}"
          case Left(error) => s"${Console.RED}It Failed :(\n${error.getMessage}${Console.RESET}"
        }

        println(s"interpreted:\t\t$interpreted")
    }
  }

  def reportError(error: lexer.TokenizationError) = {
    println(error.getMessage())
    sys.exit(65)
  }

}
