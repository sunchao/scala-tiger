package tiger.parse

import scala.util.parsing.combinator.{RegexParsers, ImplicitConversions}

class Lexer extends RegexParsers with ImplicitConversions {

  private def toSym(s: String) = Symbol(s)

  override val whiteSpace = """\s*(/(\*([^*]|\*[^/])*\*/|/[^\n]*\n)\s*)*"""r
  override val skipWhitespace = true

  private val reserved = Set("while", "for", "to", "break", "let", "in", "end", "function",
    "var", "type", "array", "if", "then", "else", "do", "of", "nil"
  )

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
  val DOT = "." ^^ toSym

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

  val STR_LIT = ("""["]([^"\\\n\r]|\\(.|\n|\r))*\\?["]"""r) ^^ { e => e}

  /** Identifiers */

  val ID : Parser[Symbol] = ("""[a-zA-Z][\w_\d]*"""r) ^^ toSym
}
