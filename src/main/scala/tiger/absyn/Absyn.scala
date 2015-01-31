package absyn

object Absyn {

  type Pos = Int

  sealed trait Exp
  sealed trait Var
  sealed trait Dec
  sealed trait Ty
  sealed trait Oper

  // Variables

  case class SimpleVar(sym: Symbol) extends Var
  case class FieldVar(variable: Var, sym: Symbol) extends Var
  case class SubscriptVar(variable: Var, exp: Exp) extends Var

  // Expressions

  case class VarExp(value: Var) extends Exp
  case class IntExp(value: Int) extends Exp
  case class StringExp(value: String) extends Exp
  case class CallExp(fname: Symbol, args: Seq[Exp]) extends Exp
  case class OpExp(left: Exp, op: Oper, right: Exp) extends Exp
  case class RecordExp(fields: Seq[(Symbol, Exp)], typ: Symbol) extends Exp
  case class SeqExp(value: Seq[Exp]) extends Exp
  case class AssignExp(variable: Var, exp: Exp) extends Exp
  case class IfExp(cond: Exp, then: Exp, els: Option[Exp]) extends Exp
  case class WhileExp(cond: Exp, body: Exp) extends Exp
  case class ForExp(variable: Symbol, var escape: Boolean, lo: Exp, hi: Exp, body: Exp) extends Exp
  case class LetExp(desc: Seq[Dec], body: Exp) extends Exp
  case class ArrayExp(typ: Symbol, size: Exp, init: Exp) extends Exp
  case object BreakExp extends Exp
  case object NilExp extends Exp

  // Declarations

  class Field(name: Symbol, var escape: Boolean, typ: Symbol)
  class FunDec(name: Symbol, params: Seq[Field], result: Option[Symbol], body: Exp)

  case class FunctionDec(value: Seq[FunDec]) extends Dec
  case class VarDec(name: Symbol, var escape: Boolean, typ: Option[Symbol], init: Exp) extends Dec
  case class TypeDec(name: Symbol, ty: Ty) extends Dec

  case class NameTy(sym: Symbol) extends Ty
  case class RecordTy(fields: Seq[Field]) extends Ty
  case class ArrayTy(sym: Symbol) extends Ty

  // Operators

  case object PlusOp extends Oper
  case object MinusOp extends Oper
  case object TimesOp extends Oper
  case object DivideOp extends Oper
  case object EqOp extends Oper
  case object NeqOp extends Oper
  case object LtOp extends Oper
  case object LeOp extends Oper
  case object GtOp extends Oper
  case object GeOp extends Oper

}
