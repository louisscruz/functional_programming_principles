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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCount(count: Int, chars: List[Char]): Boolean = {
      def updateCount(char: Char, count: Int): Int =
        if (char == '(') count + 1
        else if (char == ')') count - 1
        else count
      if (chars.isEmpty)
        if (count == 0) true else false
      else if (count < 0) false
      else balanceCount(updateCount(chars.head, count), chars.tail)
    }
    balanceCount(0, chars)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
