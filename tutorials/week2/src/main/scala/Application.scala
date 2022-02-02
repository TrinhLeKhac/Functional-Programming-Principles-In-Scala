import highOrderFunction._
import highOrderFunc._
import fixedPoint._

object Application {
  def main(args: Array[String]): Unit = {
      val a = 1
      val b = 5

      val testFixPoint = square(2.25)
      println(s"test fix point: ${testFixPoint}")
    //   val testSumSquare = sum((x: Int) => x * x)(a, b)
    //   println(s"test sum: ${testSumSquare}")

    //   val testProductSquare = product((x: Int) => x * x)(a, b)
    //   println(s"test product: ${testProductSquare}")

    //   val testMapReduce = mapReduce(map = (x: Int) => x * x, combine = (x: Int, y: Int) => x * y, zero = 1)(a, b)
    //   println(s"test map reduce: ${testMapReduce}")

    //   val testProduct2 = product2((x: Int) => x*2)(a, b)
    //   println(s"test map reduce v2: ${testProduct2}")
    //   val testid = sumId(a, b)
    //   val testcube = sumCubes(a, b)
    //   val testfactorial = sumFactorial(a, b)

    //   println(s"sum of id: ${testid}")
    //   println(s"sum of cubes: ${testcube}")
    //   println(s"sum of factorial: ${testfactorial}")

    // helper, iterate, loop, recur
  }
}
