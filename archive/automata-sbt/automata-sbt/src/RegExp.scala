
trait RegExp {
case class CharExp(c: Char) extends RegExp
case object EmptyExp extends RegExp      // 空集合
case object EpsExp extends RegExp        // ε
case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp // 連接
case class AltExp(r1: RegExp, r2: RegExp) extends RegExp    // 選択
case class StarExp(r: RegExp) extends RegExp          // 繰り返し


def RegExpToNFA[Q,A](r:RegExp):NFA[Q,A]={
    val a,z,e:RegExp
    val q0,q1:Q
    val r1,r2:RegExp
    val nfa1:NFA[Q,A] = RegExpToNFA(r1)
    val nfa2:NFA[Q,A] = RegExpToNFA(r2)
    r match { 
        case a:CharExp => 
            NFA[Q,A](
                Set[Q](q0,q1),
                Set[A](a),
                Map[(Q,Option[A]),Set[Q]]((q0,Some(a))->q1),
                q0,
                Set[Q](q1)
            )
        case z:EmptyExp =>
            NFA[Q,A](
                Set[Q](q0),
                Set[A](),
                Map[(Q,Option[A]),Set[Q]](),
                q0,
                Set[Q]()
            )
        case e:EpsExp =>
            NFA[Q,A](
                Set[Q](q0),
                Set[A](),
                Map[(Q,Option[A]),Set[Q]](),
                q0,
                Set[Q](q0)
            )
        def f(nfa:NFA[Q,A],q:Q):Map[(Q,Option[A]),Set[Q]] = {}
        case ConcatExp(r1,r2) =>
            NFA[Q,A](
                nfa1.states ++ nfa2.states,
                nfa1.alpha ++ nfa2.alpha,
                //,
                nfa1.q0,
                nfa2.finalStates
            )
        case AltExp(r1,r2) =>
            NFA[Q,A](
                Set[Q](q0) ++ nfa1.states ++ nfa2.states,
                nfa1.alpha ++ nfa2.alpha,
                Map[(Q,Option[A]),Set[Q]]((q0,None)->Set[Q](nfa1.q0,nfa2.q0)) + nfa1.transition + nfa2.transition,
                q0,
                nfa1.finalStates ++ nfa2.finalStates
            )
        case StarExp(r1) =>
            NFA[Q,A](
                Set[Q](q0) ++ nfa1.states,
                nfa1.alpha,
                //Map[(Q,Option[A]),Set[Q]]((q0,None)->nfa1.q0) + nfa1.transition,
                q0,
                Set[Q](q0)
            )
    }
}
}