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
      case AppExp(f, es) => {
        val FValue(xs,body) = fenv(f)
        val vs = Nil // Nil を正しいプログラムに置き換える必要がある
                     // map を使うと簡単
        eval(fenv, xs.zip(vs).toMap, body)
      }
    }

  def defs2env (ds: List[Def]): Map[Var, FValue] =
    ds.map(d => (d.name, FValue(d.args.map(_._1), d.body))).toMap
}


