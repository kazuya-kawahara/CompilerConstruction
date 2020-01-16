package nonscala

import Base._
import Abssyn._
import Oper._

object Eval {

  case class EvalError(s: String) extends Throwable
  def error(s: String) = throw(EvalError(s))

  // 値  
  trait Value
  case class IntVal(i: Int) extends Value
  case class BoolVal(b: Boolean) extends Value
  case class ListVal(l: List[Int]) extends Value

  // 関数
  case class FValue(xs: List[Var], e: Exp)


  def eval (fenv: Map[Var, FValue], env: Map[Var, Value], e: Exp): Value =
    e match {
      case IntExp(i) => IntVal(i)
      case NilExp => ListVal(Nil)
      case UOpExp(o, e1) => {
        val v1 = eval (fenv, env, e1)
          (o, v1) match {
          case (IsEmptyOp, ListVal(Nil)) => BoolVal(true)
          case (IsEmptyOp, ListVal(_::_)) => BoolVal(false)
          case (HeadOp, ListVal(h::t)) => IntVal(h)
          case (HeadOp, ListVal(Nil)) => error("head is applied to Nil")
          case (TailOp, ListVal(h::t)) => ListVal(t)
          case (TailOp, ListVal(Nil)) => error("tail is applied to Nil")
          case _ => error("impossible if the program is well-typed")
        }
      }
      // 以下修正
      case VarExp(x) => env(x)
      case BOpExp(o, e1, e2) => {
        val v1 = eval (fenv, env, e1)
        val v2 = eval (fenv, env, e2)
        (o, v1, v2) match {
          case (PlusOp, IntVal(i1), IntVal(i2)) => IntVal(i1+i2)
          case (MinusOp, IntVal(i1), IntVal(i2)) => IntVal(i1-i2)
          case (TimesOp, IntVal(i1), IntVal(i2)) => IntVal(i1*i2)
          case (DivideOp, IntVal(i1), IntVal(i2)) => IntVal(i1/i2)
          case (EqOp, IntVal(i1), IntVal(i2)) => BoolVal(i1==i2)
          case (LtOp, IntVal(i1), IntVal(i2)) => BoolVal(i1<i2)
          case (ConsOp, IntVal(i), ListVal(rst)) => ListVal(i::rst)
          case _ => error("impossible if the program is well-typed")
        }
      }
      case IfExp(e, e1, e2) => {
        val v = eval (fenv, env, e)
        v match {
          case BoolVal(true) => eval (fenv, env, e1)
          case BoolVal(false) => eval (fenv, env, e2)
          case _ => error("impossible if the program is well-typed")
        }
      }
      case AppExp(f, es) => {
        val FValue(xs,body) = fenv(f)
        val vs = es.map(eval(fenv, env, _))
        eval(fenv, xs.zip(vs).toMap, body)
      }
    }

  def defs2env (ds: List[Def]): Map[Var, FValue] =
    ds.map(d => (d.name, FValue(d.args.map(_._1), d.body))).toMap
}


