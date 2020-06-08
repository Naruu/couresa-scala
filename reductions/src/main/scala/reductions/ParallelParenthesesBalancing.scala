package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def helper(count: Int, index:Int, chars: Array[Char]): Boolean = {
      if(count < 0 ) false
      else if((index == chars.length) && (count == 0)) true
      else if(index >= chars.length) false
      else {
        val cur = chars(index);
        if(cur == '(') helper(count + 1, index+1, chars)
        else if (cur == ')') helper(count - 1, index +1, chars)
        else helper(count, index + 1, chars)
      }
      }
      helper(0, 0, chars)
    }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, a1_until : Int, a2_start: Int): Int = {
      var a1 = idx + a1_until -1
      var a2 = a2_start

      while (a1 >= idx && a2 < until && chars(a1) == '(' && chars(a2) == ')') {
        a1 -= 1
        a2 += 1
      }

      while (a2 < until) {
        chars(a1+1) = chars(a2)
        a2 += 1
        a1 += 1
      }
      a1 + 1 - idx + 1
    }
  

    def reduce(from: Int, until: Int): Int = {
      if((until - from) <= threshold) {
        var idx = from
        var i = from
        while(i < until){
          if(chars(i) != '(' && chars(i) != ')') i += 1
          else if ((i+1) < until && chars(i) == '(' && chars(i+1) == ')') i += 2
          else {
            chars(idx) = chars(i)
            idx += 1
            i += 1
          }
        }
        idx - from
      }
      else {
        val mid = (from + until)/2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        traverse(from, mid+r, l, mid+1)
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
