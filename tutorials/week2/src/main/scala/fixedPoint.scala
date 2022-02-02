object fixedPoint {
  val tolerance: Double = 0.0001
  def abs(x: Double): Double = if(x > 0) x else -x
  def isGoodEnough(x: Double, y: Double): Boolean = abs(x - y) < tolerance
  def fixPoint(f: Double => Double)(firstGuest: Double): Double = {
      def iterate(guest: Double): Double = {
          val next: Double = f(guest)
          if(isGoodEnough(guest, next)) guest else iterate(next)
      }
      iterate(firstGuest)
  }
  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x))/2
  def square(x: Double) = fixPoint((averageDamp((y: Double) => x/y)))(1.0)
}
