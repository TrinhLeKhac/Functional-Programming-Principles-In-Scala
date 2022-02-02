object Application {
  def main(args: Array[String]): Unit = {
      val a = new Rational(14, 16)
      val b = new Rational(3, 4)
    //   val c = a.add(b)
    //   val d = a + b
    //   val e = a.+(b)
    // //   val e = a.min(b) infix def min => a.min(b) error
    //   val f = a min b // only use a min b
      println(a)
  }
}
