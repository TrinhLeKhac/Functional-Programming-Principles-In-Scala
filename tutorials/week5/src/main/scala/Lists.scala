trait Lists[T]

class Nil extends Lists[Nothing]:
    def isEmpty = true
    def head: T = throw Error("no head")
    def tail: Lists[T] = throw Error("no tail")
    
class Cons(val head: T, val tail: Lists[T]) extends Lists[T]:
    def isEmpty = false

object Lists[T] {
    
}
extension[T](xs: Lists[T])
    def ++[T](ys: Lists[T]): Lists[T] = xs match {
        case Nil => ys
        case x::xs => x :: (xs ++ ys)
    }
    def reverse[T]: Lists[T] = xs match {
        case Nil => Nil
        case x::xs => xs.reverse ++ List(x)
    }