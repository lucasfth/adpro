// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Cons(_, ax) => ax
    case _ => throw NoSuchElementException()

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] = l match
    case _ if n <= 0 => l
    case Cons(_, ax) => drop(ax, n-1)
    case _ => throw NoSuchElementException()

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Cons(a, ax) if p(a) => dropWhile(ax, p)
    case _ => l

  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = l match
    case Cons(_, Nil) => Nil
    case Cons(a, ax) => Cons(a, init(ax))
    case _ => throw NoSuchElementException()

  // Exercise 6

  def length[A] (l: List[A]): Int = 
    List.foldRight(l, 0, (_, acc) => acc+1 )

  // Exercise 7

  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Cons(a, ax) => foldLeft(ax, f(z, a), f)
    case _ => z

  // Exercise 8

  def product (as: List[Int]): Int = foldLeft(as, 1, (_*_))

  def length1[A] (as: List[A]): Int = foldLeft(as, 0, (acc, _) => acc+1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = foldLeft(l, Nil, (elem, acc) => Cons(acc, elem))
 
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = foldLeft(reverse(l), z, (a, b) => f(b,a))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B =
    foldRight(l, (b: B) => b, (elem, acc) => b => acc(f(b, elem)))(z)
 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = foldLeft(l, Nil, append)
  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] =
    foldRight(l, Nil, (elem, acc) => if p(elem) then Cons(elem, acc) else acc)
 
  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = concat(map(l,f))

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = flatMap(l,elem => if p(elem) then Cons(elem, Nil) else Nil)

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = (l, r) match
    case (Cons(a, ax),Cons(b, bx)) => Cons((a+b), addPairwise(ax,bx))
    case _ => Nil

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = (l, r) match
    case (Cons(a, ax),Cons(b, bx)) => Cons(f(a,b), zipWith(ax,bx,f))
    case _ => Nil

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean =
    @annotation.tailrec
    def isSub(sup: List[A], sub: List[A]): Boolean = (sup,sub) match
      case (_, Nil) => true
      case (Cons(a,ax), Cons(b,bx)) if a == b => isSub(ax,bx)
      case _ => false
    
    @annotation.tailrec
    def containsSub(sup: List[A], sub: List[A]): Boolean = sup match
      case Nil => sub == Nil
      case _ if isSub(sup,sub) => true
      case Cons(_,ax) => containsSub(ax,sub)
  
    containsSub(sup,sub)
