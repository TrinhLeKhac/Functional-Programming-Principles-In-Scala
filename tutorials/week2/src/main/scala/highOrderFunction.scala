object highOrderFunction {

  def id(x: Int): Int = x

  def cube(x: Int): Int = x * x * x

  def factorial(x: Int): Int = if(x == 0) 1 else x*factorial(x-1)

  def sumOf(f: Int => Int, x: Int, y: Int): Int = {
      if(x > y) 0 else f(x) + sumOf(f, x+1, y)
  }

  def sumId(x: Int, y: Int): Int = sumOf(id, x, y)
  def sumCubes(x: Int, y: Int): Int = sumOf(cube, x, y)
  def sumFactorial(x: Int, y: Int): Int = sumOf(factorial, x, y)
}
