// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

import scala.util.boundary

// Exercise 1

trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int =
    if (this.x < that.x || (this.x == that.x && this.y < that.y)) -1
    else if (this.x == that.x && this.y == that.y) 0
    else +1

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int = t match
    case Branch(l, r) => 1 + size(l) + size(r)
    case _ => 1

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(n) => n

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(n) => Leaf(f(n))

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Branch(l, r) => f(fold(l)(f)(g),fold(r)(f)(g))
    case Leaf(n) => g(n)

  def size1[A](t: Tree[A]): Int =
    fold(t)((l: Int, r: Int) => 1 + l + r)(_ => 1)

  def maximum1(t: Tree[Int]): Int =
    fold(t)((l: Int, r: Int) => l max r)(n => n)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((l: Tree[B],r: Tree[B]) => Branch(l,r))(n => Leaf(f(n)))


enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case Some(v) => Some(f(v))
    case None => None

  def getOrElse[B >: A] (default: => B): B = this match
    case Some(v) => v
    case None => default

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(v) => f(v)
    case None => None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case Some(v) => Some(v)
    case None => ob

  def filter(p: A => Boolean): Option[A] = this match
    case Some(v) if p(v) => Some(v)
    case _ => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)
    



object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    for
      o1 <- ao
      o2 <- bo
    yield f(o1,o2)

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight(Some(Nil))((elem, acc) => 
        for
          x <- elem
          xs <- acc
        yield x :: xs
      )

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil))((elem, acc) =>
      for
        x <- f(elem)
        xs <- acc
      yield x :: xs
      )
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h :: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] = 
  headOption(lst.map(_._2))


def headGrade1(lst: List[(String,Int)]): Option[Int] =
  for 
    (name, grade) <- headOption(lst)
  yield grade

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))

def variance1(xs: Seq[Double]): Option[Double] =
  for
    m <- mean(xs)
    v <- mean(xs.map(x => math.pow(x-m,2)))
  yield v
 
// Scroll up, to the Option object for Exercise 9
