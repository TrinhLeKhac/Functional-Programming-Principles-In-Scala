// abstract class can not instantiate
// can has abstract members(fields, methods)

// Set collections within a Tree

trait IntSet: 
    def isEmpty: Boolean
    def contains(x: Int): Boolean
    def include(x: Int): IntSet
    def union(t: IntSet): IntSet
end IntSet
object IntSet: 
    def apply() = Empty
    def apply(x: Int) = Empty.include(x)
    def apply(x: Int, y: Int) = Empty.include(x).include(y)
end IntSet

object Empty extends IntSet:
    def isEmpty = true
    def contains(x: Int) = false
    def include(x: Int) = NonEmpty(x, Empty, Empty)
    def union(t: IntSet) = t
end Empty

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def isEmpty = false
    def contains(x: Int): Boolean = {
        if(x < elem) left.contains(x)
        else if(x > elem) right.contains(x)
        else true
    }
    def include(x: Int): IntSet = {
        if(x < elem) NonEmpty(elem, left.include(x), right)
        else if(x > elem) NonEmpty(elem, left, right.include(x))
        else this
    }
    def union(t: IntSet): IntSet = {
        left.union(t).union(right).include(elem)
    }
end NonEmpty

