// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*
import scala.util.boundary


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (v, nextRng) = rng.nextInt
    val nonNegInt = if v < 0 then v*(-1) else v
    (nonNegInt, nextRng)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (nonNegInt, nextRng) = nonNegativeInt(rng)
    val doubleVal = nonNegInt.toDouble / (Int.MaxValue.toDouble + 1)
    (doubleVal, nextRng)

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    var (nonNegInt, nextRng) = nonNegativeInt(rng)
    val (doubleVal, nnextRng) = double(nextRng)
    ((nonNegInt, doubleVal), nnextRng)

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (nonNegInt, nextRng) = nonNegativeInt(rng)
    val (doubleVal, nnextRng) = double(nextRng)
    ((doubleVal, nonNegInt), nnextRng)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    (0 until size).foldRight(List.empty[Int], rng)((_, acc) =>
      val (lst, curRng) = acc
      val (v, nextRng) = nonNegativeInt(curRng)
      (v :: lst, nextRng)
    ) match {case (lst, finRng) => (lst.reverse, finRng)}

  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (v1, nextRng) = ra(rng)
      val (v2, nnextRng) = rb(nextRng)
      (f(v1,v2), nnextRng)
    }

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List.empty[A]))((elem, acc) =>
      map2(elem, acc)(_ :: _)
    )

  def ints2(size: Int): Rand[List[Int]] =
    sequence(List.fill(size)(nonNegativeInt))

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => 
      val (v, nextRng) = f(rng)
      g(v)(nextRng)

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt)(v => unit(v % bound))

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
   State {s =>
      val (a, newS) = run(s)
      f(a).run(newS)
   }

  def map[B](f: A => B): State[S, B] = // Got help from atro
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for
      a <- this
      b <- sb
    yield f(a, b)

object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit(List.empty[A]))((elem, acc) =>
      elem.map2(acc)(_ :: _)
    )

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    val (a, newS) = s.run(initial)
    cons(a, stateToLazyList(s)(newS))

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] =
    val v = State[RNG, Int](_.nextInt)
    stateToLazyList(v)(rng)

  lazy val tenStrictInts: List[Int] =
    lazyInts(RNG.SimpleRNG(0)).take(10).toList

end State
