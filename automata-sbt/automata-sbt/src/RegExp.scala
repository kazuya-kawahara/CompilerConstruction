
trait RegExp
case class CharExp(c: Char) extends RegExp
case object EmptyExp extends RegExp      // 空集合
case object EpsExp extends RegExp        // ε
case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp // 連接
case class AltExp(r1: RegExp, r2: RegExp) extends RegExp    // 選択
case class StarExp(r: RegExp) extends RegExp          // 繰り返し


