import scala.annotation.tailrec

sealed trait Computation[A]
class Continue[A](n: => Computation[A]) extends Computation[A] {
  lazy val next = n
}
case class Done[A](result: A) extends Computation[A]

def even(i: Int): Computation[Boolean] = i match {
  case 0 => Done(true)
  case _ => new Continue(odd(i - 1))
}

def odd(i: Int): Computation[Boolean] = i match {
  case 0 => Done(false)
  case _ => new Continue(even(i - 1))
}

@tailrec
def run[A](computation: Computation[A]): A = computation match {
  case Done(a) => a
  case c: Continue[A] => run(c.next)
}

run(even(5000))

import scala.util.control.TailCalls._

def isEven(xs: List[Int]): TailRec[Boolean] =
  if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))

def isOdd(xs: List[Int]): TailRec[Boolean] =
  if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))

isEven((1 to 100000).toList).result

def fib(n: Int): TailRec[Int] =
  if (n < 2) done(n) else for {
    x <- tailcall(fib(n - 1))
    y <- tailcall(fib(n - 2))
  } yield (x + y)

fib(40).result
