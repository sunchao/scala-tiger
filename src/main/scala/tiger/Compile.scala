package tiger

import _root_.absyn.Absyn
import tiger.parse.Parser

import scala.io.Source


object Compile {
  def main(args: Array[String]) : Unit = {
    val src = Source.fromFile(args(0)).mkString
    val ast = parse(src)
  }

  private def parse(input: String): Option[Absyn.Exp] = {
    val parser = new Parser
    val result = parser.parse(input)
    result match {
      case Some(ast) => {
        println(ast)
        Some(ast)
      }
      case None => {
        println("Parse failed.")
        None
      }
    }
  }
}
