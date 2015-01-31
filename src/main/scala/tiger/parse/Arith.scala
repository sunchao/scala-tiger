package tiger.parse

import scala.util.parsing.combinator.JavaTokenParsers

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}


object ParseExpr extends Arith {
  def main(args: Array[String]): Unit = {
    println("input: " + args)
    println(parseAll(expr, args(0)))
  }
}