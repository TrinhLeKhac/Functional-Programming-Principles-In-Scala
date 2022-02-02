/**
 * Cons(1, Cons(2, Cons(3, Nil)))
 * 1: head
 * Cons(2, Cons(3, Nil)): tail
 * syntactic sugar: List(1, 2, 3)
 * ::(cons-cell)
 * tree
 *                        ::
 *                       /  \
 *                      1    ::            
 *                          /  \ 
 *                         2   ::
 *                            /  \
 *                           3   Nil
 * ========> 1::(2::(3::Nil)) <=> 1::2::3::Nil(evaluate from right to left)
 * 
 * Array and List, items homogeneous
 * Decompose list with pattern matching
 * 
 * Sorting list x::xs
 * Sorting xs(recursive) and insert x in right position
*/

class InsertionSort {
    def sort(xs: List[Int]): List[Int] = xs match {
        case Nil => Nil
        case x::xs1 => insert(x, sort(xs1))
    }
    def insert(y: Int, xs: List[Int]): List[Int] = xs match {
        case Nil => List(y)
        case x::xs1 => if(y <= x) y::xs else x::insert(y, xs)
    }
}
