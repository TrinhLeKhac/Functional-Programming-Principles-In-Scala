package example

// object Application {
//   import List._
//   def main(args: Array[String]): Unit = {
//     val a = sum(List(1, 2, 3))
//     val b = max(List(4, 3, 1, 10, 2))
//     println(a)
//     println(b)
//   }
// }

object Application extends App {
  import Lists._
  val a = sum(List(1, 2, 3))
  val b = sum(List())
  // val c = Lists.max(List())
  val d = max(List(1, 2, 3))
  println(a)
}
