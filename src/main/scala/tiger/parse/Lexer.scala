package tiger.parse

import scala.util.parsing.combinator.{RegexParsers, ImplicitConversions}

class Lexer extends RegexParsers with ImplicitConversions {

  private def toSym(s: String) = Symbol(s)

  /** Keywords */

  val WHILE = "while" ^^ toSym
  val FOR = "for" ^^ toSym
  val TO = "to" ^^ toSym
  val BREAK = "break" ^^ toSym
  val LET = "let" ^^ toSym
  val IN = "in" ^^ toSym
  val END = "end" ^^ toSym
  val FUNCTION = "function" ^^ toSym
  val VAR = "var" ^^ toSym
  val TYPE = "type" ^^ toSym
  val ARRAY = "array" ^^ toSym
  val IF = "if" ^^ toSym
  val THEN = "then" ^^ toSym
  val ELSE = "else" ^^ toSym
  val DO = "do" ^^ toSym
  val OF = "of" ^^ toSym
  val NIL = "nil" ^^ toSym

  /** Operators */

  val PLUS = "+" ^^ toSym
  val MINUS = "-" ^^ toSym
  val TIMES = "*" ^^ toSym
  val DIVIDE = "/" ^^ toSym
  val EQ = "=" ^^ toSym
  val NEQ = "<>" ^^ toSym
  val LT = "<" ^^ toSym
  val LE = "<=" ^^ toSym
  val GT = ">" ^^ toSym
  val GE = ">=" ^^ toSym
  val AND = "&" ^^ toSym
  val OR = "|" ^^ toSym
  val ASSIGN = ":=" ^^ toSym

  /** Punctuation Symbols */

  val LBRACE = "{" ^^ toSym
  val RBRACE = "}" ^^ toSym
  val LPAREN = "(" ^^ toSym
  val RPAREN = ")" ^^ toSym
  val LBRACK = "[" ^^ toSym
  val RBRACK = "]" ^^ toSym
  val COLON = ":" ^^ toSym

  /** Integer Literals */

  val INT_LIT = ("""\d+""".r) ^^ { _ toInt }

  /** String Literals */

  val STR_LIT = ("""["]([^"\\\n\r]|\\(.|\n|\r))*\\?["]"""r) ^^ identity

  /** Identifiers */

  var ID = ("""[a-z][\w_\d]*"""r) ^^ toSym
}
