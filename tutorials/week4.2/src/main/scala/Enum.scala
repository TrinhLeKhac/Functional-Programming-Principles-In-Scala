import java.util.GregorianCalendar
import java.lang.ProcessBuilder.Redirect
/**
 * Enum using with case: compose and decompose pure data without any associated functions
 * algebraic data types (ADT)
 * move all class/ case class extends from trait to companion object
 * => not pollute type(class/trait) namespace
 * => move all to object(value namespace)
 * trait/class and object in 2 different namespace(type and value)
*/

// trait Expr 
// object Expr:
//     case class Number(n: Int) extends Expr 
//     case class Var(x: String) extends Expr 
//     case class Sum(e1: Expr, e2: Expr) extends Expr 
//     case class Prod(e1: Expr, e2: Expr) extends Expr
// end Expr

enum Expr:
    case Number(n: Int)
    case Var(x: String)
    case Sum(e1: Expr, e2: Expr)
    case Prod(e1: Expr, e2: Expr)

// Pattern matching on enum
import Expr._
def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
}

def showP(e: Expr): String = e match {
    case _: Sum => s"(${show(e)})"
    case _ => show(e)
}

enum Color: 
    case Red 
    case Green 
    case Blue

enum dayOfWeek:
    case Mon, Tue, Wed, Thu, Fri, Sat, Sun

/**
 * enum have parameter 
 * name of enum(capitalize)
 * name of case of enum(capitalize)
 * values(values of enum)
 * ordinal(ordinal number from 0(index of enum))
*/

enum Direction(val dx: Int, val dy: Int):
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)
    case Up extends Direction(0, 1)
    case Down extends Direction(0, -1)

    def leftTurn = Direction.values((ordinal+1)/4)
end Direction

val r = Direction.Right
val u = r.leftTurn
val z = (u.dx, u.dy)
