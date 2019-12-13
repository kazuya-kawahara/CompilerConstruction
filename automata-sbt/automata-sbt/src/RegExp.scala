
trait RegExp {
case class CharExp(c: Char) extends RegExp
case object EmptyExp extends RegExp      // 空集合
case object EpsExp extends RegExp        // ε
case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp // 連接
case class AltExp(r1: RegExp, r2: RegExp) extends RegExp    // 選択
case class StarExp(r: RegExp) extends RegExp          // 繰り返し


def RegExpToNFA[Q,A](r:RegExp):NFA[Q,A]={
    val a,empty,eps:RegExp
    val q0,q1:Q
    val r1,r2:RegExp

    r match { 
        case a:CharExp => 
            NFA[Q,A](
                Set[Q](q0,q1),
                Set[A](a),
                Map[(Q,Option[A]),Set[Q]]((q0,Some(a))->q1),
                q0,
                Set[Q](q1)
            )
        case empty:EmptyExp =>
            NFA[Q,A](
                Set[Q](q0),
                Set[A](),
                Map[(Q,Option[A]),Set[Q]](),
                q0,
                Set[Q]()
            )
        case eps:EpsExp =>
            NFA[Q,A](
                Set[Q](q0),
                Set[A](),
                Map[(Q,Option[A]),Set[Q]](),
                q0,
                Set[Q](q0)
            )
        case ConcatExp(r1,r2) =>
            NFA[Q,A](
                RegExpToNFA(r1).states ++ RegExpToNFA(r2).states,
                RegExpToNFA(r1).alpha ++ RegExpToNFA(r2).alpha,
                //わからない部分 Map[(Q,Option[A]),Set[Q]]((q0,None)->Set[Q](RegExpToNFA(r1).q0,RegExpToNFA(r2).q0)) + RegExpToNFA(r1).transition + RegExpToNFA(r2).transition,
                ,
                RegExpToNFA(r1).q0,
                RegExpToNFA(r2).finalStates
            )
        case AltExp(r1,r2) =>
            NFA[Q,A](
                Set[Q](q0) ++ RegExpToNFA(r1).states ++ RegExpToNFA(r2).states,
                RegExpToNFA(r1).alpha ++ RegExpToNFA(r2).alpha,
                Map[(Q,Option[A]),Set[Q]]((q0,None)->Set[Q](RegExpToNFA(r1).q0,RegExpToNFA(r2).q0)) + RegExpToNFA(r1).transition + RegExpToNFA(r2).transition,
                q0,
                RegExpToNFA(r1).finalStates ++ RegExpToNFA(r2).finalStates
            )
        case StarExp(r1) =>
            NFA[Q,A](
                Set[Q](q0) ++ RegExpToNFA(r1).states,
                RegExpToNFA(r1).alpha,
                //わからない部分 Map[(Q,Option[A]),Set[Q]]((q0,None)->RegExpToNFA(r1).q0) + RegExpToNFA(r1).transition,
                ,
                q0,
                Set[Q](q0)
            )
    }
}
}