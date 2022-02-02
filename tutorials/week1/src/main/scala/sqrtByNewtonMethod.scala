object sqrtByNewtonMethod {
  // calculate absolute of double x
    def abs(x: Double): Double = if(x < 0) -x else x

    // square def
    def square(x: Double): Double = x * x

    // isGoodEnough
    def isGoodEnough(guess: Double, x: Double): Boolean = abs(square(guess) - x) < 0.0001

    // improve def
    def improve(guess: Double, x: Double): Double = (guess + x/guess)/2

    // sqrtIter
    def sqrtIter(guess: Double, x: Double): Double = {
        if(isGoodEnough(guess, x)) guess
        else sqrtIter(improve(guess, x), x)
    }

    def sqrt(x: Double): Double = {
        assert(x >= 0, "We can get sqrt of non negative double")
        sqrtIter(1.0, x)
    }
}
