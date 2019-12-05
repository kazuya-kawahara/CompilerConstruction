import Abssyn._
import Base._

object Eval {
  def eval (env: Map[Var, Int], e: Exp) : Int =
    e match {
      case VarExp(x) => {
        def option2int(x:Option[Int]) = {
          x match {
            case Some(i) => i
            case None => 0
          }
        }
        option2int(env.get(x))
      } //revised
      case IntExp(i) => i
      case PlusExp(e1, e2) => eval (env, e1) + eval (env, e2)
      case MinusExp(e1, e2) => eval (env, e1) - eval (env, e2)
      case TimesExp(e1, e2) => eval (env, e1) * eval (env, e2)
      case DevideExp(e1, e2) => eval (env, e1) / eval (env, e2)
    }
}



