// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*
import scala.collection.View.Empty

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2

  property("Ex02.01: headOption does not force the tail of a lazy list") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))

    forAll { (n: Int) => cons(n, ???).headOption; true }
  
  // Exercise 3

  property("Ex03.01: take does not force any heads nor any tails of the lazy list it manipulates") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))

    forAll { (n: Int) => cons(???, ???).take(n); true }

  // Exercise 4

  def nLazy[A](n: Int)(elem: => A)(endElem: => A): LazyList[A] =
    if (n < 0) then empty
    else if (n == 1) then cons(endElem, empty)
    else cons(elem, nLazy(n-1)(elem)(endElem))

  property("Ex04.01: take(n) does not force the (n+1)st head ever (even if we force all elements of take(n))") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))

    forAll { (n: Int) => nLazy(n+1)(n)(???).take(n).toList; true }

  // Exercise 5

  property("Ex05.01: l.take(n).take(n) == l.take(n) for any lazy list s and any n") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int, ll: LazyList[Int]) =>
      ll.take(n).take(n).toList == ll.take(n).toList
    }
  
  // Exercise 6

  property("Ex06.01: l.drop(n).drop(m) == l.drop(n+m) for any n, m") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (m: Int, n: Int, ll: LazyList[Int]) =>
      ll.drop(n).drop(m).toList == ll.drop(n+m).toList
    }
  
  // Exercise 7

  property("Ex07.01: l.drop(n) does not force any of the dropped elements (heads)") =
    given Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))

    forAll { (m: Int, n: Int) => nLazy(m)(???)(???).drop(n); true }

  // Exercise 8

  property("Ex08.01: l.map(identity) == l for any lazy list l") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (ll: LazyList[Int]) => ll.map(identity).toList == ll.toList }

  // Exercise 9

  property("Ex09.01: map terminates on infinite lazy lists") =
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll { (ll: LazyList[Int]) => ll.map(identity); true }
 
  // Exercise 10

  property("Ex10.01: append terminates on two infinite lazy lists") =
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll { (ll: LazyList[Int]) => ll.append(ll); true }

  property("Ex10.02: append does not change order of elements") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (ll1: LazyList[Int], ll2: LazyList[Int]) =>
      ll1.toList ::: ll2.toList == ll1.append(ll2).toList
    }
