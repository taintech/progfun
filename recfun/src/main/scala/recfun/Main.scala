package recfun
import common._

import scala.annotation.tailrec

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
    val SEED = List(1)
    @tailrec
    def loop(list: List[Int], counter: Int): Int = {
      if(counter == 0) list(c)
      else loop(transform(list, SEED), counter-1)
    }
    loop(SEED, r)
  }

  @tailrec
  def transform(list: List[Int], res: List[Int]): List[Int] = {
    list match {
      case x::Nil => x::res
      case x::xs => transform(xs,(x+xs.head)::res)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(ar: List[Char], balance: Int): Boolean =
      if(balance<0) false
      else {
        ar match {
          case '('::xs => loop(xs,balance+1)
          case ')'::xs => loop(xs, balance-1)
          case Nil if balance==0 => true
          case _ => balance==0
        }
      }
    loop(chars.filter(c => c=='('||c==')'),0)
  }

  def checkBorders(chars: List[Char]) = {
    chars.nonEmpty&&chars.head=='('&&chars.reverse.head==')'
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty||money<coins.min) 0
    else{
      val bases = coins.toSet.filter(_<money).toList.sorted
      val n = bases.size
      val multipliers = bases.map(money/_)
      val m = List.fill(n)(0)
      @tailrec
      def loop(list: List[Int], count: Int = 0): Int = {
        if(list.isEmpty) count
        else if(list.zip(bases).map{case (a,b) => a*b}.sum==money)
          loop(next(list), count+1)
        else loop(next(list), count)
      }
      def next(list: List[Int]): List[Int] = {
        list.zipWithIndex.find{
          case (a,b) => a < multipliers(b)
        }.map{
          case (a,0) => list.patch(0, Seq(a+1), 1)
          case (a,b) => list.patch(0, List.fill(b)(0):::a+1::Nil, b+1)
        }.getOrElse{
          List.empty
        }
      }
      loop(m)
    }
  }
}
