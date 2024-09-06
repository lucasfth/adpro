// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n * n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int =
  @annotation.tailrec
  def aux(n: Int, cur: Int, acc: Int): Int =
    if n == 0 then cur
    else if n == 1 then acc
    else aux(n-1, acc, acc+cur)
  aux(n-1, 0, 1)

// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def aux(arr: Array[A]): Boolean =
    if arr.length <= 1 then true
    else if ordered(arr.head, arr(1)) then aux(arr.tail)
    else false

  if as.length <= 1 then true
  else aux(as)
    

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(isSortedCurried)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))
