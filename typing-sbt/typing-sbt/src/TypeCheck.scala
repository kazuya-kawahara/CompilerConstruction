package nonscala
import Base._
import Abssyn._
import Oper._
import Ty._

object TypeCheck {

  case class TypeError(s: String) extends Throwable
  def typeError(s: String) = throw(TypeError(s))

  case class FuncTy(ts: List[Ty], result: Ty)

  // 式 e の型を返す
  // 型エラーがあった場合は, 例外 TypeError を発生させる
  // tcheck
  def tcheck (fenv: Map[Var, FuncTy], env: Map[Var, Ty], e: Exp) : Ty =
    e match {
      case VarExp(x) =>
        env.get(x) match {
          case Some(t) => t
          case None => typeError(s"Cannot find variable: $x")
        }
      case UOpExp(o, e1) => {
        val t1 = tcheck(fenv, env, e1)
        (o, t1) match {
          case (IsEmptyOp, IntListTy) => BoolTy
          case (HeadOp, IntListTy) => IntTy
          case (TailOp, IntListTy) => IntListTy
          case _ => typeError("UOpExp")
        }
      }
      //以下追加
      case IntExp(i) =>
        env.get(i) match {
          case Some(i) => i
          case None => typeError(s"Cannot find integer: $i")
        }
      case NilExp => {
        IntListTy
      }
      case BOpExp(o, e1, e2) => {
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        (o, t1, t2) match {
          case (PlusOp, IntTy, IntTy)|(MinusOp, IntTy, IntTy)|(TimesOp, IntTy, IntTy)|(DivideOp, IntTy, IntTy) => IntTy
          case (EqOp, IntTy, IntTy)|(LtOp, IntTy, IntTy) => BoolTy
          case (ConsOp, IntTy, IntListTy) => IntListTy
          case _ => typeError("BOpExp")
        }
      }
      //以下未完
      case IfExp(e, e1, e2) => {
        val t = tcheck(fenv, env, e)
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        (t, t1, t2) match {
          case (BoolTy, Prog, Prog)
        }
      }
      case AppExp(f, es) => {
        val FuncTy(ts,t) = fenv(f)
        ... 
        // Listのmapを使うと簡単            
      }

    }

  def defs2fenv (ds: List[Def]): Map[Var, FuncTy] =
    ds.map(d =>
      (d.name, FuncTy(d.args.map(_._2), d.rtype))).toMap

  // 型エラーがあった場合は, 例外 TypeError を発生させる
  def tcheckDefs (ds: List[Def]): Unit = {
    val fenv = defs2fenv(ds)
    for (d <- ds)
      if (tcheck(fenv, d.args.toMap, d.body) != d.rtype)
        typeError(s"Type error: ${d.name}")

  }
}
