package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if((c < 0) || (r < 0) || (c > r)) 0
    else if((c==0)||(c==r)) 1 
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * pascal tail-rec
   * pascal(c, r) = pascal(c-1, r-1) + pascal(c, r-1)
   * accumulator not apply
   * 
   * 
   * 
   * idea: assign all possible cases in list
  */

  // def pascal(c: Int, r: Int): Int = {
  //   def recur(sum: Int, li: List[Int, Int]): Int = li match {
  //       case Nil => sum
  //       case (c, r)::xs => {
  //       if((c < 0) || (r < 0) || (c > r)) recur(sum, xs)
  //       else if((c == 0) || (c == r)) recur(sum+1, xs)
  //       else recur(sum, (c-1, r-1)::(c, r-1)::xs)
  //       }
  //   recur(0, List((c, r)))
  //   }
  // }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def recur(chars: List[Char], opens: Int): Boolean = {
      if(chars.isEmpty) (opens == 0)
      else if(chars.head == '(') recur(chars.tail, opens + 1)
      else if(chars.head == ')') (opens > 0) && recur(chars.tail, opens - 1)
      else recur(chars.tail, opens)
    }
    recur(chars, 0)
  }

  /**
   * using foldLeft with core (i: Int, b: Boolean), i: balance between '(' and ')', b: break condition
   * foldLeft contains recursive, not need recur anymore
  */
  /**
   * def foldLeft[A](z: A, xs: List[B])(f: (A, B) => A): A = {
   *    if(xs.isEmpty) z
   *    else
   *    foldLeft(f(z, xs.head), xs.tail)(f)
   * }
   * 
   * def foldLeft[A](z: A)(xs: List[B])(f: (A, B) => A): A = {
   *    if(xs.isEmpty) z
   *    else
   *    foldLeft(f(z, xs.head))(xs.tail)(f)
   * }
   * 
  */
  // def balance(chars: Char[Int]): Boolean = {
  //   val (cond, opens): (Boolean, Int) = chars.foldLeft((true, 0)){
  //     case ((cond, opens), char) => char match {
  //       case '(' => (cond, opens + 1)
  //       case ')' => ((opens > 0) && cond, opens - 1)
  //       case _ => (cond, opens)
  //     }
  //   }
  //   cond && (opens == 0)
  // }

  /**
   * val n == if else if else opens, wrapping in count function
  */

  // def balance(chars: List[Char]): Boolean = {
  //   def count(ch: Char, opens: Int): Int = {
  //     if(ch == '(') opens + 1
  //     else if(ch == ')') opens - 1
  //     else opens
  //   }
  //   def recur(chars: List[Char], opens: Int): Boolean = {
  //     if(chars.isEmpty) opens == 0
  //     else {
  //       val n = count(chars.head, opens)
  //       (n >= 0) && recur(chars.tail, n)
  //     }
  //   }
  //   recur(chars, 0)
  // }

  /**
   * val n == if else if else opens
  */
  // def balance(chars: List[Char]): Boolean = {
  //   def recur(chars: List[Char], opens: Int): Boolean = {
  //     if(chars.isEmpty) opens == 0
  //     else{
  //       val n = {
  //         if(chars.head == '(') opens + 1
  //         else if(chars.head == ')') opens - 1
  //         esle opens
  //       }
  //       // if(n >= 0) recur(chars.tail, n) else false
  //       (n >= 0) && recur(chars.tail, n)
  //     }
  //   }
  //   recur(chars, 0)
  // }

  /**
   * Pattern matching
  */
  // def balance(chars: List[Char]): Boolean = {
  //   def recur(chars: List[Char], opens: Int): Boolean = (chars, opens) match {
  //     case (Nil, opens) => opens == 0
  //     case (head::tail, opens) => {
  //       val n = {
  //       if(head == '(') opens + 1
  //       else if(head == ')') opens - 1
  //       else opens
  //       }
  //       (n >= 0) && recur(tail, n)
  //     }
  //   }
  //   recur(chars, 0)
  // }

  /**
   * Using mapping
  */
  // def balance(chars: List[Char]): Boolean = {
  //   val m = Map('(' -> 1, ')' -> -1).withDefaultValue(0)
  //   def recur(chars: List[Char], opens: Int): Int = chars match {
  //     case head::tail if opens >= 0 => recur(tail, m(head) + opens)
  //     case _ => opens
  //   }
  //   recur(chars, 0) == 0
  // }

  /**
   * recur(li, acc) => recur(li.tail, f(acc)) (core)
   * recur(li, acc) => cond(li.head, acc) && recur(li.tail, f(li.head, acc))
  */



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recur(money: Int, coins: List[Int]): Int = {
    if((coins.isEmpty)||(money < 0)) 0 // case money < 0 return 0 => handle loop infinity when money - coin_a -.... < 0
    else if(money == 0) 1 // money - coin_a - coin_b - ... = 0 => money = coin_a + coin_b + ... => 1 solution
    else recur(money - coins.head, coins) + recur(money, coins.tail)
    }
    recur(money, coins.sorted)
  }


  /**
   * 
   * tail-rec by idea in pascal triangle
  */

  // def countChange(money: Int, coins: List[Int]): Int = {
  //   def recur(acc: Int, li: List[Int, Int]): Int = li match {
  //     case Nil => acc
  //     case (money, coins)::xs => {
  //       if((money < 0) || (coins.isEmpty)) recur(acc, xs)
  //       else if(money == 0) recur(acc + 1, xs)
  //       else recur(acc, (money - coins.head, coins)::(money, coins.tail)::li)
  //     }
  //   recur(0, List((money, coins)))
  //   }
  // }