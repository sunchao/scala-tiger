package tiger.parse

import absyn.Absyn
import absyn.Absyn.{Exp, FunctionDec, FunDec}

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}
import scala.util.parsing.input.CharArrayReader

class Parser extends Lexer with ImplicitConversions with PackratParsers {

  def parse(input: String): Option[Absyn.Exp] = {
    val result: ParseResult[Exp] = phrase(program)(new CharArrayReader((input.toCharArray)))
    result match {
      case Success(result, _) => Some(result)
      case NoSuccess(err, next) => {
        val message = "Error: failed to parse input (line %d, column %d), reason: \n\t%s".
          format(next.pos.line, next.pos.column, err)
        println(message)
        None
      }
    }
  }

  /** Program */

  lazy val program : PackratParser[Absyn.Exp] = exp

  /** Declarations */

  lazy val typfield : PackratParser[Absyn.Field] = (ID <~ COLON) ~ ID ^^ flatten2 {
    (name, typ) => new Absyn.Field(name, false, typ)
  }

  lazy val typ : PackratParser[Absyn.Ty] = (
    (ARRAY ~ OF) ~> ID ^^ Absyn.ArrayTy
  | LBRACE ~> repsep(typfield, ",") <~ RBRACE ^^ Absyn.RecordTy
  | ID ^^ Absyn.NameTy
  )

  lazy val tydec : PackratParser[Absyn.Dec] = (TYPE ~> ID) ~ (EQ ~> typ) ^^ flatten2 {
    (name, typ) => Absyn.TypeDec(name, typ)
  }

  lazy val vardec : PackratParser[Absyn.Dec] = (
    VAR ~> ID ~ (ASSIGN ~> exp) ^^ flatten2 {
      (name, init) => Absyn.VarDec(name, false, None, init)
    }
  | VAR ~> ID ~ (COLON ~> ID) ~ (ASSIGN ~> exp) ^^ flatten3 {
      (name, typ, init) => Absyn.VarDec(name, false, Some(typ), init)
    }
  )

  lazy val fundec : PackratParser[Absyn.Dec] = (
    FUNCTION ~> ID ~ (LPAREN ~> repsep(typfield, ",") <~ RPAREN ~ EQ) ~ exp ^^ flatten3 {
      (name, params, body) => new FunctionDec(List(new FunDec(name, params, None, body)))
    }
  | FUNCTION ~> ID ~ (LPAREN ~> repsep(typfield, ",") <~ RPAREN ~ COLON) ~ ID ~ (EQ ~> exp) ^^ flatten4 {
      (name, params, typ, body) => new FunctionDec(List(new FunDec(name, params, Some(typ), body)))
    }
  )

  lazy val dec : PackratParser[Absyn.Dec] = (tydec | vardec | fundec)

  lazy val decs : PackratParser[Seq[Absyn.Dec]] = rep(dec)

  /** Expressions */

  lazy val literal = (STR_LIT ^^ Absyn.StringExp | INT_LIT ^^ Absyn.IntExp)

  lazy val funcall = ID ~ (LPAREN ~> repsep(exp, ",") <~ RPAREN) ^^ flatten2 {
    (fname, args) => Absyn.CallExp(fname, args)
  }

  lazy val binop = (
    exp ~ (PLUS ~> exp) ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.PlusOp, right)
    }
  | (exp <~ MINUS) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.MinusOp, right)
    }
  | (exp <~ TIMES) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.TimesOp, right)
    }
  | (exp <~ DIVIDE) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.DivideOp, right)
    }
  | (exp <~ EQ) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.EqOp, right)
    }
  | (exp <~ NEQ) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.NeqOp, right)
    }
  | (exp <~ GT) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.GtOp, right)
    }
  | (exp <~ GE) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.GeOp, right)
    }
  | (exp <~ LT) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.LtOp, right)
    }
  | (exp <~ LE) ~ exp ^^ flatten2 {
      (left, right) => Absyn.OpExp(left, Absyn.LeOp, right)
    }
  | (exp <~ AND) ~ exp ^^ flatten2 {
      (left, right) => Absyn.IfExp(left, right, Some(Absyn.IntExp(0)))
    }
  | (exp <~ OR) ~ exp ^^ flatten2 {
      (left, right) => Absyn.IfExp(left, Absyn.IntExp(1), Some(right))
    }
  )

  lazy val field = (ID <~ EQ) ~ exp ^^ flatten2 {
    (name, exp) => (name, exp)
  }

  lazy val record = ID ~ (LBRACE ~> repsep(field, ",") <~ RBRACE) ^^ flatten2 {
    (typ, fields) => Absyn.RecordExp(fields, typ)
  }
  
  lazy val array = ID ~ (LBRACK ~> exp <~ RBRACK ~ OF) ~ exp ^^ flatten3 {
    (typ, size, init) => Absyn.ArrayExp(typ, size, init)
  }

  lazy val ifthenelse = IF ~> exp ~ (THEN ~> exp <~ ELSE) ~ exp ^^ flatten3 {
    (cond, _then, _else) => Absyn.IfExp(cond, _then, Some(_else))
  }

  lazy val ifthen = (IF ~> exp) ~ (THEN ~> exp) ^^ flatten2 {
    (cond, _then) => Absyn.IfExp(cond, _then, None)
  }

  lazy val loop = (WHILE ~> exp) ~ (DO ~> exp) ^^ flatten2 {
    (cond, body) => Absyn.WhileExp(cond, body)
  }

  lazy val forloop = (FOR ~> ID <~ ASSIGN) ~ exp ~ (TO ~> exp <~ DO) ~ exp ^^ flatten4 {
    (id, lo, hi, body) => Absyn.ForExp(id, false, lo, hi, body)
  }

  lazy val let = (LET ~> decs) ~ (IN ~> repsep(exp, ";") <~ END) ^^ flatten2 {
    (decs, exps) => Absyn.LetExp(decs, Absyn.SeqExp(exps))
  }

  lazy val negexp = MINUS ~> exp ^^ {
    e => Absyn.OpExp(Absyn.IntExp(0), Absyn.MinusOp, e)
  }

  lazy val assignment = lvalue ~ (ASSIGN ~> exp) ^^ flatten2 {
    (lv, exp) => Absyn.AssignExp(lv, exp)
  }

  lazy val sequence = LPAREN ~> repsep(exp, ";") <~ RPAREN ^^ {
    es => Absyn.SeqExp(es)
  }

  lazy val nil = NIL ^^^ Absyn.NilExp

  lazy val break = BREAK ^^^ Absyn.BreakExp

  /** lvalue - left recursion */
  lazy val lvalue : PackratParser[Absyn.Var] = (
    lvalue ~ (LBRACK ~> exp <~ RBRACK) ^^ flatten2 {
      (lv, exp) => Absyn.SubscriptVar(lv, exp)
    }
  | lvalue ~ (DOT ~> ID) ^^ flatten2 {
      (lv, sym) => Absyn.FieldVar(lv, sym)
    }
  | ID ^^ Absyn.SimpleVar
  )

  lazy val exp : PackratParser[Absyn.Exp] = (
    sequence // LPAREN
  | let // LET
  | binop
  | ifthenelse // IF
  | ifthen // IF
  | loop // WHILE
  | forloop // FOR
  | break
  | record // ID LBRACE
  | array // ID LBRACK
  | assignment // lvalue ..
  | negexp // MINUS
  | funcall // ID LPAREN
  | lvalue ^^ Absyn.VarExp
  | literal // ID
  )

}
