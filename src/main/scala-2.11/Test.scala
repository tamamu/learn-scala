/**
  * Created by tamamu on 5/24/16.
  */

import datastructures.List2

object Test {

  // Fibonacci
  private def fib(n: Int): Int = {
    def f(k: Int): Int = k match {
      case 0 => 0
      case 1|2 => 1
      case _ => f(k - 2) + f(k - 1)
    }
    f(n)
  }

  // Format result of Int function
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // Find item position by Boolean function from the array
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n:Int):Int=
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  // Check whether the array is sorted
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n:Int):Boolean=
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def un_curry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))

  def main(args: Array[String]) {
    /*
    println(formatResult("Fibonacci", 5, fib))
    println(findFirst(Array("Hello", "World", "!"), (x:String) => x == "!"))
    println(isSorted(Array(2, 1), (a:Int, b:Int) => a <= b))

    println(curry((x:Int, y:Int) => x+y)(3)(5))
    println(un_curry(curry((a:Int, b:Int) => a-b))(10, 5))
    println(compose(
      (x:Int) => x*x,
      (x:Int) => x+5
    )(5))
    */

    println(List2.reverseLeft(List2(1, 2, 3, 4, 5)))
    println(List2.foldLeft(List2(1, 2, 3, 4, 5), 0)(_ + _))
  }
}
