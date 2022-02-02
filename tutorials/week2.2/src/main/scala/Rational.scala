class Rational(a: Int, b: Int) {
  // require enforces a precondition on the caller of function, throw IllegalArgumentException
  // assert check condition inside the function, throw AssertionError
  require(b > 0, "denominator must be positive")

  private def gcd(a: Int, b: Int): Int = {
  if(b == 0) a else gcd(b, a%b)
  }

  val numer = a/gcd(a.abs, b)
  val denom = b/gcd(a.abs, b)

  // auxiliary constructor
  // method with name this
  def this(r: Int) = this(r, 1)

  def add(r: Rational) = Rational(this.numer * r.denom + this.denom * r.numer, this.denom * r.denom)

  def neg(r: Rational) = Rational(-r.numer, r.denom)
  def sub(r: Rational) = add(neg(r))

  def mul(r: Rational) = Rational(this.numer * r.numer, this.denom * r.denom)

  def less(r: Rational): Boolean = (this.numer * r.denom < this.denom * r.numer)
  def greater(r: Rational): Boolean = !this.less(r)

  override def toString = s"${this.numer}/${this.denom}"
}

/**
 * def f(ps1)(ps2)(ps3)..(psn) = E
 * def f(ps1)(ps2)...(psn-1) = {g(psn) = E; g}
 * def f(ps1)(ps2)...(psn-1) = (psn => E)
 * def f = (ps1 => (ps2 => (...(psn => E))))
 * curry function
 * 
 * 
*/

// using +, -, * as a method name
// alphanumeric identifiers
// symbolic identifiers
// precedence
// method using not in internal of class
// can define in extension outside of class

extension (x: Rational)
  def + (y: Rational): Rational = x.add(y)
  def - (y: Rational) = x.sub(y)
  def * (y: Rational) = x.mul(y)
  def < (y: Rational) = x.less(y)
  def > (y: Rational) = x.greater(y)
  infix def min(y: Rational) = if(x.less(y)) x else y
  infix def max(y: Rational) = if(x.less(y)) y else x