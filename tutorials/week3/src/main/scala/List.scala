trait List[T] {
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
}

class NIL[T] extends List[T] {
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.tail")
    def isEmpty = true
}

class CONS[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
}