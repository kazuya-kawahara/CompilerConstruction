import Base._
import Oper._
import Abssyn._
import Tokens._

class Parser (val src: Yylex) {

  var tok: Token = src.yylex()

  def advance () =  tok = src.yylex()

  def eat (t: Token) = 
    if (tok == t) advance() else error()

  def F(): Exp =
    tok match {
      case NIL => advance(); NilExp
      case INT(i) => advance(); IntExp(i)
      case ID(s) => advance(); VarExp(s)
      case LPAREN =>
        {
          eat(LPAREN)
          val e = E()
          eat(RPAREN)
          e
        }
      case _ => error()
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL =>  Tprime(F())
      case _ => error()
    }

  def Tprime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); Tprime(BOpExp(TimesOp, e, F()))
      case DIV => eat(DIV); Tprime(BOpExp(DivideOp, e, F()))
      case PLUS | MINUS| RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON => e
      case _ => error()
    }

  def E(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => Eprime(T())
      case _ => error()
    }

  def Eprime(e: Exp): Exp = 
    tok match {
      case PLUS => eat(PLUS); Eprime(BOpExp(PlusOp, e, T()))
      case MINUS => eat(MINUS); Eprime(BOpExp(MinusOp, e, T()))
      case TIMES | DIV | RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON => e
      case _ => error()
    }

  def C(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => 
    }

  def B(): Exp =
    tok match {
      
    }

  def I(): Exp = C()  // プログラムを書く．
}

