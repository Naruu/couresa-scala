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
    if ((c==0) || (r==0)) 1
    else pascal(c-1, r-1) + pascal(c, r)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char]): Boolean = 
      if(chars.isEmpty) false
      else {
        if(chars.head == ')') true
        else balance2(chars.tail)
      }
    if(chars.isEmpty) true
    else {
      if(chars.head == ')') balance(chars.tail)
      else balance2(chars.tail)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
