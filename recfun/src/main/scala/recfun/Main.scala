package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance(")".toList))
    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case (_, 0) => 0
      case (i, j) => pascal(i - 1, j - 1) + pascal(i, j - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance0(chars: List[Char], depth: Int = 0): Boolean = {
      if(chars.isEmpty) {
        depth == 0
      }
      else if(depth < 0) false
      else if(chars.head == '(') balance0(chars.tail, depth + 1)
      else if(chars.head == ')') balance0(chars.tail, depth - 1)
      else balance0(chars.tail, depth)
    }
    if(chars.isEmpty) false
    else balance0(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(capacity: Int, changes: List[Int]): Int = {
      if(capacity == 0) 1
      else if(capacity < 0) 0
      else if(changes.isEmpty && capacity>=1 ) 0
      else count(capacity, changes.tail) + count(capacity - changes.head, changes)
    }
    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
