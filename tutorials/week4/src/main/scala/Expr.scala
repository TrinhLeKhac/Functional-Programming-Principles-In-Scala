/**
 * remove fields and method from trait
 * move fields from class to constructor of class(case class)
 * same method from class extends trait, define 1 times(pattern matching)
 * move method to extension(e: Expr):
*/

trait Expr

object Expr {
case class Num(n: Int) extends Expr
case class Var(x: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
    case Num(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
    case _ => throw Error("no match")
}

def show(e: Expr): String = e match {
    case Num(n) => n.toString
    case Var(s) => s
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
}

def showP(e: Expr): String = e match {
    case e: Sum => s"(${show(e)})"
    case _ => show(e)
    }

}



// trait Expr:
//     def isNumber: Boolean
//     def isVar: Boolean
//     def isSum: Boolean
//     def isProd: Boolean
//     def numValue: Int
//     def varName: String
//     def leftOp: Expr
//     def rightOp: Expr
//     def eval: Int
// end Expr

// class Number(n: Int) extends Expr:
//     def isNumber = true
//     def isVar = false
//     def isSUm = false
//     def isProd = false
//     def numValue = n
//     def varName = throw Error("varName")
//     def leftOp = throw Error("leftOp")
//     def rightOp = throw Error("rightOp")
//     def eval: Int = n
// end Number

// class Var(x: String) extends Expr:
//     def isNumber = false
//     def isVar = true
//     def isSUm = false
//     def isProd = false
//     def numValue = throw Error("numValue")
//     def varName = x
//     def leftOp = throw Error("leftOp")
//     def rightOp = throw Error("rightOp")
//     def eval = throw Error("not valid")
// end Var

// class Sum(e1: Expr, e2: Expr) extends Expr: 
//     def isNumber = false
//     def isVar = false
//     def isSum = true
//     def isProd = false
//     def numValue = throw Error("numValue")
//     def leftOp = e1
//     def rightOp = e2
//     def eval: Int = e1.eval + e2.eval
// end Sum 

// class Prod(e1: Expr, e2: Expr) extends Expr:
//     def isNumber = false
//     def isVar = false
//     def isSum = false
//     def isProd = true
//     def numValue = throw Error("numValue")
//     def leftOp = e1
//     def rightOp = e2
//     def eval: Int = e1.eval * e2.eval
// end Prod



