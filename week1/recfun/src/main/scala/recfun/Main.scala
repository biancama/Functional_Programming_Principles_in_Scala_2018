package recfun

import scala.util.control.TailCalls._

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
    def pascalNoTail(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascalNoTail(c - 1, r - 1) + pascalNoTail(c , r - 1)

    def pascal(c: Int, r: Int): Int = {
      def pascalApp(c: Int, r: Int): TailRec[Int] = if (c == 0 || r == c) done(1) else for {
        x <- tailcall(pascalApp(c - 1, r - 1))
        y <- tailcall(pascalApp(c, r - 1))
      } yield (x + y)
      pascalApp(c, r).result
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(chars: List[Char], open: Int):Boolean = {
        if (open < 0) false
        else {
          chars match {
            case x :: xs => if (x == '(') balance(xs, open + 1) else if (x == ')') balance(xs, open - 1) else balance(xs, open)
            case _ => open == 0
          }
        }
      }
      balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChangeNoTail(money: Int, coins: List[Int]): Int = {
      //printf("money: %s, coins: [%s]\n", money, coins.mkString(","))
      if (money == 0) 1 else {
        coins match {
          case x :: xs => if (x <= money) countChangeNoTail(money - x, x :: xs) + countChangeNoTail(money, xs) else countChangeNoTail(money, xs)
          case Nil => 0
        }
      }
    }

    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange(money: Int, coins: List[Int]):TailRec[Int] = {
        (money, coins) match {
          case (0, _) => done(1)
          case(_, Nil) => done(0)
          case(c, x::xs) if (x <= money) => for {
            withX <- tailcall(countChange(c - x, x::xs))
            withoutX <- tailcall(countChange(c, xs))
          } yield (withX + withoutX)
          case(c, x::xs) => tailcall(countChange(c, xs))
        }
      }
      countChange(money, coins).result
    }
  }
