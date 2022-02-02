package idealized

trait Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
    def isZero = true
    def predecessor: Nat = ???
    def successor: Nat = Succ(this)
    def +(that: Nat): Nat = that
    def -(that: Nat): Nat = if(that.isZero) this else ???
    override def toString = "Zero"
}

class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = Succ(this)
    def +(that: Nat): Nat = Succ(n + that)
    def -(that: Nat): Nat = if(that.isZero) this else ???
    override def toString = s"Succ($n)"
}
