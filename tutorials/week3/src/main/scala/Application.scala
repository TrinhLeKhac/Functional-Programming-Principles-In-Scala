object Application {

    import List._

    def main(args: Array[String]): Unit = {
        val a = singleton[Int](1)
        val b = doubleton[Int](1, 2)
        val li = CONS[Int](1, CONS[Int](2, CONS[Int](3, NIL[Int])))
        val c = nth(li, 3)
        println(c)
    }
    def singleton[T](elem: T): List[T] = CONS[T](elem, NIL[T])
    def doubleton[T](elem1: T, elem2: T): List[T] = CONS[T](elem1, CONS[T](elem2, NIL[T]))

    def nth[T](xs: List[T], n: Int): T = {
        if(xs.isEmpty) throw new IndexOutOfBoundsException
        else if(n == 0) xs.head
        else nth(xs.tail, n-1)
    }
}
