package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if ((c == 0) || (r == c)) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], count: Int): Boolean = {
      if(chars.isEmpty) {
        if (count == 0) true
        else false
      }
      else {
        if (count < 0) false
        else {
          if (chars.head == '(') balance2(chars.tail, count + 1)
          else {
            if(chars.head == ')') balance2(chars.tail, count - 1)
            else balance2(chars.tail, count)
          }           
        }
      }
    }
    if(chars.isEmpty) true
    else balance2(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def counter(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else {
        if (money == 0) 1
        else {
          if (money >= coins.head) counter(money - coins.head, coins) + counter(money, coins.tail)
          else counter(money, coins.tail)
        }
      }
    }
    counter(money, coins)
  }
}