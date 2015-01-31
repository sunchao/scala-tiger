package tiger.parse

import absyn.Absyn
import absyn.Absyn.{FunctionDec, FunDec}

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}

class Parser extends Lexer with ImplicitConversions with PackratParsers {

  /** Program */

  def program : Parser[Absyn.Exp] = exp

  /** Declarations */

  def typfield : Parser[Absyn.Field] = ID ~ COLON ~ ID ^^ { case name~COLON~typ => new Absyn.Field(name, false, typ) }

  def typ : Parser[Absyn.Ty] = (
    ID ^^ { case ty => Absyn.NameTy(ty) }
      | LBRACE ~ rep(typfield) ~ RBRACE ^^ { case LBRACE~fs~RBRACE => Absyn.RecordTy(fs) }
      | ARRAY ~ OF ~ ID ^^ { case ARRAY~OF~typ => Absyn.ArrayTy(typ) }
    )

  def tydec : Parser[Absyn.Dec] = TYPE ~ ID ~ EQ ~ typ ^^ { case TYPE~name~EQ~typ => Absyn.TypeDec(name, typ) }

  def vardec : Parser[Absyn.Dec] = (
    VAR ~ ID ~ ASSIGN ~ exp ^^ { case VAR~name~ASSIGN~init => Absyn.VarDec(name, false, None, init) }
      | VAR ~ ID ~ COLON ~ ID ~ ASSIGN ~ exp ^^ { case VAR~name~COLON~typ~ASSIGN~init => Absyn.VarDec(name, false, Some(typ), init) }
    )

  def fundec : Parser[Absyn.Dec] = (
    FUNCTION ~ ID ~ LPAREN ~ rep(typfield) ~ RPAREN ~ EQ ~ exp ^^ { case FUNCTION~name~LPAREN~params~RPAREN~EQ~body => new FunctionDec(List(new FunDec(name, params, None, null))) }
      | FUNCTION ~ ID ~ LPAREN ~ rep(typfield) ~ RPAREN ~ COLON ~ ID ~ EQ ~ exp ^^ { case FUNCTION~name~LPAREN~params~RPAREN~COLON~typ~EQ~body => new FunctionDec(List(new FunDec(name, params, Some(typ), null))) }
    )

  def dec : Parser[Absyn.Dec] = (tydec | vardec | fundec)

  def decs : Parser[Seq[Absyn.Dec]] = rep(dec)

  /** Expressions */

  def literal = (STR_LIT ^^ Absyn.StringExp | INT_LIT ^^ Absyn.IntExp)

  def funcall = ID ~ LPAREN ~ repsep(exp, ",") ~ RPAREN ^^ { case fname~LPAREN~args~RPAREN => Absyn.CallExp(fname, args) }

  def binop = (
    exp ~ PLUS ~ exp ^^ { case left~PLUS~right => Absyn.OpExp(left, Absyn.PlusOp, right) }
      | exp ~ MINUS ~ exp ^^ { case left~MINUS~right => Absyn.OpExp(left, Absyn.MinusOp, right) }
      | exp ~ TIMES ~ exp ^^ { case left~TIMES~right => Absyn.OpExp(left, Absyn.TimesOp, right) }
      | exp ~ DIVIDE ~ exp ^^ { case left~DIVIDE~right => Absyn.OpExp(left, Absyn.DivideOp, right) }
      | exp ~ EQ ~ exp ^^ { case left~EQ~right => Absyn.OpExp(left, Absyn.EqOp, right) }
      | exp ~ NEQ ~ exp ^^ { case left~NEQ~right => Absyn.OpExp(left, Absyn.NeqOp, right) }
      | exp ~ GT ~ exp ^^ { case left~GT~right => Absyn.OpExp(left, Absyn.GtOp, right) }
      | exp ~ GE ~ exp ^^ { case left~GE~right => Absyn.OpExp(left, Absyn.GeOp, right) }
      | exp ~ LT ~ exp ^^ { case left~LT~right => Absyn.OpExp(left, Absyn.LtOp, right) }
      | exp ~ LE ~ exp ^^ { case left~LE~right => Absyn.OpExp(left, Absyn.LeOp, right) }
      | exp ~ AND ~ exp ^^ { case left~AND~right => Absyn.IfExp(left, right, Some(Absyn.IntExp(0))) }
      | exp ~ OR ~ exp ^^ { case left~OR~right => Absyn.IfExp(left, Absyn.IntExp(1), Some(right)) }
    )

  def field = ID ~ EQ ~ exp ^^ { case typ~EQ~exp => (typ, exp) }

  def record = ID ~ LBRACE ~ repsep(field, ",") ~ RBRACE ^^ { case typ~LBRACE~fields~RBRACE => Absyn.RecordExp(fields, typ) }
  
  def array = ID ~ LBRACK ~ exp ~ RBRACK ~ OF ~ exp ^^ { case typ~LBRACK~size~RBRACK~OF~init => Absyn.ArrayExp(typ, size, init) }

  def ifthenelse = IF ~ exp ~ THEN ~ exp ~ ELSE ~ exp ^^ { case IF~cond~THEN~then~ELSE~els => Absyn.IfExp(cond, then, Some(els)) }

  def ifthen = IF ~ exp ~ THEN ~ exp ^^ { case IF~cond~THEN~then => Absyn.IfExp(cond, then, None) }

  def loop = WHILE ~ exp ~ DO ~ exp ^^ { case WHILE~cond~DO~body => Absyn.WhileExp(cond, body) }

  def forloop = FOR ~ ID ~ ASSIGN ~ exp ~ TO ~ exp ~ DO ~ exp ^^ { case FOR~id~ASSIGN~lo~TO~hi~DO~body => Absyn.ForExp(id, false, lo, hi, body) }

  def let : Parser[Absyn.Exp] = LET ~ decs ~ IN ~ repsep(exp, ";") ~ END ^^ { case LET~decs~IN~exps~END => Absyn.LetExp(decs, Absyn.SeqExp(exps)) }

  def negexp = MINUS ~> exp ^^ { case e => Absyn.OpExp(Absyn.IntExp(0), Absyn.MinusOp, e) }

  def assignment = lvalue ~ ASSIGN ~ exp ^^ { case lv~ASSIGN~exp => Absyn.AssignExp(lv, exp) }

  def sequence : Parser[Absyn.SeqExp] = LPAREN ~> repsep(exp, ";") <~ RPAREN ^^ { es => Absyn.SeqExp(es) }

  def nil = NIL ^^^ Absyn.NilExp

  def break = BREAK ^^^ Absyn.BreakExp

  /** lvalue - left recursion */
  lazy val lvalue : Parser[Absyn.Var] = (
    ID ^^ { case v => Absyn.SimpleVar(v) }
      | lvalue ~ ID ^^ { case lv~sym => Absyn.FieldVar(lv, sym) }
      | lvalue ~ LBRACK ~ exp ~ RBRACK ^^ { case lv~LBRACK~exp~RBRACK => Absyn.SubscriptVar(lv, exp) }
    )

  lazy val exp : Parser[Absyn.Exp] = (
    literal
      | negexp
      | record
      | sequence
      | funcall
      | assignment
      | binop
      | array
      | ifthenelse
      | ifthen
      | loop
      | forloop
      | let
      | break
      | lvalue ^^ { case lv => Absyn.VarExp(lv) }
    )

}
