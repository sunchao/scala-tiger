package tiger.absyn

import java.io.{OutputStream, PrintWriter}

import absyn.Absyn._

/**
 * A class that print the abstract-syntax tree in a particular way
 */
trait PrintAbsyn {
  def print(e: Exp): Unit
}

/**
 * A simple implementation of [[PrintAbsyn]] that pretty-print AST
 * by indent level.
 * @param output output for the AST
 */
class SimplePrintAbsyn(val output: OutputStream) extends PrintAbsyn {
  private val out: PrintWriter = new PrintWriter(output)

  def print(e: Exp): Unit = {
    out.println("====================== AST ======================")
    printExp(e, 0);
    out.println()
    out.println("================= END OF AST ====================")
    out.flush()
  }

  def indent(i: Int): Unit = i match {
    case x if x > 0 =>  { out.print("  "); indent(i-1) }
    case 0 => ()
  }

  def doList[A](es: Seq[A], d: Int, f: (A, Int) => Unit): Unit = {
    es match {
      case Nil => ()
      case e :: Nil =>
        out.println()
        f(e, d+1)
      case e :: es =>
        out.println()
        f(e, d+1)
        out.print(",")
        doList(es, d, f)
    }
  }

  def printVar(v: Var, d: Int): Unit = v match {
    case SimpleVar(sym) =>
      indent(d)
      out.print("SimpleVar(")
      out.print(sym.name)
      out.print(")")
    case FieldVar(_var, sym) =>
      indent(d)
      out.println("FieldVar(")
      printVar(_var, d+1)
      out.println(",")
      indent(d+1)
      out.print(sym.name)
      out.print(")")
    case SubscriptVar(_var, exp) =>
      indent(d)
      out.println("SubscriptVar(")
      printVar(_var, d+1)
      out.println(",")
      printExp(exp, d+1)
      out.print(")")
  }

  private def printField(field: Field, d: Int): Unit = {
    indent(d)
    out.print("(")
    out.print(field.name)
    out.print(",")
    out.print(field.escape)
    out.print(",")
    out.print(field.typ)
    out.print(")")
  }

  private def printFunDec(fundec: FunDec, d: Int): Unit = {
    indent(d)
    out.print("(")
    out.print(fundec.name)
    out.print(",[")
    doList(fundec.params, d, printField)
    out.println("],")
    indent(d+1)
    fundec.result match {
      case None => out.print("NONE")
      case Some(sym) =>
        out.print("SOME(")
        out.print(sym.name)
        out.print(")")
    }
    out.println(",")
    printExp(fundec.body, d+1)
    out.print(")")
  }

  def printDec(c: Dec, d: Int): Unit = c match {
    case FunctionDec(value) =>
      indent(d)
      out.print("FunctionDec[")
      doList(value, d, printFunDec)
      out.print("]")
    case VarDec(name, escape, typ, init) =>
      indent(d)
      out.print("VarDec(")
      out.print(name.name)
      out.print(",")
      out.print(escape)
      out.print(",")
      typ match {
        case None => out.print("NONE")
        case Some(sym) =>
          out.print("SOME(")
          out.print(sym.name)
          out.print(")")
      }
      out.println(",")
      printExp(init, d+1)
      out.print(")")
    case TypeDec(name, ty) =>
      indent(d)
      out.print("TypeDec(")
      out.print(name.name)
      out.println(",")
      printTy(ty, d+1)
      out.print(")")
  }

  def printTy(ty: Ty, d: Int): Unit = {
    ty match {
      case NameTy(sym) =>
        indent(d)
        out.print("NameTy(")
        out.print(sym.name)
        out.print(")")
      case RecordTy(fields) =>
        indent(d)
        out.print("RecordTy[")
        doList(fields, d, printField)
        out.print("]")
      case ArrayTy(sym) =>
        indent(d)
        out.print("ArrayTy(")
        out.print(sym.name)
        out.print(")")
    }
  }

  def printExp(e: Exp, d: Int): Unit = e match {
    case VarExp(_var) =>
      indent(d)
      out.println("VarExp(")
      printVar(_var, d+1)
      out.print(")")
    case IntExp(value) =>
      indent(d)
      out.print("IntExp(")
      out.print(value.toString)
      out.print(")")
    case StringExp(value) =>
      indent(d)
      out.print("StringExp(")
      out.print(value)
      out.print(")")
    case CallExp(fname, args) =>
      indent(d)
      out.print("CallExp(")
      out.print(fname.name)
      out.print(",[")
      doList(args, d, printExp)
      out.print("])")
    case OpExp(left, op, right) =>
      indent(d)
      out.print("OpExp(")
      out.print(op)
      out.println(",")
      printExp(left, d+1)
      out.println(",")
      printExp(right, d+1)
      out.print(")")
    case RecordExp(fields, typ) =>
      indent(d)
      out.print("RecordExp(")
      out.print(typ.name)
      out.print(",[")
      doList(fields, d, (e: (Symbol, Exp), d: Int) => e match {
        case (sym: Symbol, exp: Exp) =>
          indent(d)
          out.print("(")
          out.print(sym.name)
          out.println(",")
          printExp(exp, d + 1)
          out.print(")")
      })
      out.print("]")
    case SeqExp(value) =>
      indent(d)
      out.print("SeqExp[")
      doList(value, d, printExp)
      out.print("]")
    case AssignExp(_var, exp) =>
      indent(d)
      out.println("AssignExp(")
      printVar(_var, d+1)
      out.println(",")
      printExp(exp, d+1)
      out.print(")")
    case IfExp(cond, _then, _else) =>
      indent(d)
      out.println("IfExp(")
      printExp(cond, d+1)
      out.println(",")
      printExp(_then, d+1)
      _else match {
        case None => ()
        case Some(e) =>
          out.println(",")
          printExp(e, d+1)
      }
    case WhileExp(cond, body) =>
      indent(d)
      out.println("WhileExp(")
      printExp(cond, d+1)
      out.println(",")
      printExp(body, d+1)
      out.print(")")
    case ForExp(_var, escape, lo, hi, body) =>
      indent(d)
      out.println("ForExp(")
      indent(d+1)
      out.print(_var.name)
      out.print(",")
      out.print(escape)
      out.println(",")
      printExp(lo, d+1)
      out.println(",")
      printExp(hi, d+1)
      out.println(",")
      printExp(body, d+1)
      out.print(")")
    case LetExp(decs, body) =>
      indent(d)
      out.print("LetExp([")
      doList(decs, d, printDec)
      out.println("],")
      printExp(body, d+1)
      out.print(")")
    case ArrayExp(typ, size, init) =>
      indent(d)
      out.print("ArrayExp(")
      out.print(typ.name)
      out.println(",")
      printExp(size, d+1)
      out.println(",")
      printExp(init, d+1)
      out.print(")")
    case BreakExp =>
      indent(d)
      out.print("BreakExp")
    case NilExp =>
      indent(d)
      out.print("NilExp")
  }
}

