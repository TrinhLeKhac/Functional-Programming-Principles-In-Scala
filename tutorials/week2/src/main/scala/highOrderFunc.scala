object highOrderFunc {

//   def sum(f: Int => Int)(a: Int, b: Int): Int = {
//       if(a > b) 0 else f(a) + sum(f)(a + 1, b)
//   }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
      def loop(a: Int, acc: Int): Int = {
          if(a > b) acc
          else 
              loop(a + 1, f(a) + acc)
      }
      loop(a, 0)
  }

//   def product(f: Int => Int)(a: Int, b: Int): Int = {
//       if(a > b) 1 else f(a) * product(f)(a + 1, b)
//   }

def product(f: Int => Int)(a: Int)(b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
        if(a > b) acc
        else
            loop(a + 1, f(a) * acc)
    }
    loop(a, 1)
}

  def fact(n: Int): Int = product((x: Int) => x)(1)(n)
  def factSquare(n: Int): Int = product((x: Int) => x * x)(1)(n)

  def factv1 = product((x: Int) => x)(1) _ // curry

  // general of sum and product

//   def mapReduce(map: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
//       def loop(a: Int): Int = {
//       if(a > b) zero else combine(map(a), loop(a+1))
//       }
//       loop(a)
//   }

def mapReduce(zeroPoint: Int)(map: Int => Int)(combine: (Int, Int) => Int)(a: Int)(b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
        if(a > b) acc
        else
            loop(a + 1, combine(map(a), acc))
    }
    loop(a, zeroPoint)
}

  def product2(f: Int => Int) = mapReduce(1)(f)((x: Int, y: Int) => x * y) _
  
}
