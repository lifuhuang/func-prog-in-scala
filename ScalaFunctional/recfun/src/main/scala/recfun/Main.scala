package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) {
        1
      } else if (c < 0 || c > r) {
        0
      } else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], unclosed: Int): Boolean = {
        if (chars.isEmpty) {
          unclosed == 0
        } else if (chars.head == '(') {
          loop(chars.tail, unclosed + 1)
        } else if (chars.head == ')') {
          if (unclosed > 0) loop(chars.tail, unclosed - 1) else false
        } else {
          loop(chars.tail, unclosed)
        }
      }
      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (coins.isEmpty || money < 0) {
        0
      } else {
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }
    }
  }
