object Application {
  import Expr._
  def main(args: Array[String]): Unit = {
      val a = Sum(Num(1), Num(2))
      print(eval(a))
  }
}
