// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

// Note: we are using our own lazy lists, not the standard library

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h,t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => t()

  /* Note 1. f can return without forcing the tail
   *
   * Note 2. this is not tail recursive (stack-safe) It uses a lot of stack if
   * f requires to go deeply into the lazy list. So folds sometimes may be less
   * useful than in the strict case
   *
   * Note 3. We added the type C to the signature. This allows to start with a
   * seed that is a subtype of what the folded operator returns.
   * This helps the type checker to infer types when the seed is a subtype, for 
   * instance, when we construct a list:
   *
   * o.foldRight (Nil) ((a,z) => a:: z)
   *
   * The above works with this generalized trick. Without the C generalization
   * the compiler infers B to be List[Nothing] (the type of Nil) and reports
   * a conflict with the operator.  Then we need to help it like that:
   *
   * o.foldRight[List[Int]] (Nil) ((a,z) => a:: z)
   *
   * With the C type trick, this is not neccessary. As it hints the type
   * checker to search for generalizations of B.
   *
   * I kept the foldLeft type below in a classic design, so that you can
   * appreciate the difference. Of course, the same trick could've been
   * applied to foldLeft.
   */
  def foldRight[B, C >: B](z: => B)(f: (A, => C) => C): C = this match
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))

  /* Note 1. Eager; cannot be used to work with infinite lazy lists. So
   * foldRight is more useful with lazy lists (somewhat opposite to strict lists)
   * Note 2. Even if f does not force z, foldLeft will continue to recurse.
   */
  def foldLeft[B](z: => B)(f :(A, => B) => B): B = this match
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)

  // Note: Do you know why we can implement find with filter for lazy lists but
  // would not do that for regular lists?
  def find(p: A => Boolean) = 
    this.filter(p).headOption

  // Exercise 2

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(a, ax) => a() :: ax().toList

  def toList1: List[A] = foldRight(Nil)(_::_) // Done with help from atro

  // Test in the REPL, for instance: LazyList(1,2,3).toList 
  // (and see what list is constructed)

  // Exercise 3

  def take(n: Int): LazyList[A] = (this, n) match
    case (Empty, _) => Empty
    case (_, 0) => Empty
    case (Cons(a, ax), _) => cons(a(), ax().take(n-1))

  def drop(n: Int): LazyList[A] = (this, n) match
    case (Empty, _) => this
    case (_, 0) => this
    case (Cons(_, ax), _) => ax().drop(n-1)

  // Exercise 4

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(a, ax) if p(a()) => cons(a(), ax().takeWhile(p))
    case _ => Empty

  // Exercise 5
  
  def forAll(p: A => Boolean): Boolean = this match
    case Cons(a, ax) if p(a()) => ax().forAll(p)
    case Cons(a, _) if !p(a()) => false
    case _ => true
 
  // Note 1. lazy; tail is never forced if satisfying element found this is
  // because || is non-strict
  // Note 2. this is also tail recursive (because of the special semantics
  // of ||)

  def exists(p: A => Boolean): Boolean = this match
      case Cons(a, _) if p(a()) => true
      case Cons(a, ax) if !p(a()) => ax().exists(p)
      case _ => false

  // Exercise 6
  
  def takeWhile1(p: A => Boolean): LazyList[A] =
    this.foldRight(Empty)((elem, acc) => if p(elem) then cons(elem, acc) else Empty)

  // Exercise 7

    // def headOption: Option[A] = this match
    // case Empty => None
    // case Cons(h,t) => Some(h())
  
  def headOption1: Option[A] =
    this.foldRight(None)((elem, _) => Some(elem))

  // Exercise 8
  
  // Note: The type is incorrect, you need to fix it
  def map[B](f: A => B): LazyList[B] =
    this.foldRight(Empty)((elem, acc) => cons(f(elem), acc))

  // Note: The type is incorrect, you need to fix it
  def filter(p: A => Boolean): LazyList[A] = 
    this.foldRight(Empty)((elem, acc) => if p(elem) then cons(elem, acc) else acc)

  /* Note: The type is given correctly for append, because it is more complex.
   * Try to understand the type. The constraint 'B >: A' requires that B is a
   * supertype of A. The signature of append allows to concatenate a list of
   * supertype elements, and creates a list of supertype elements.  We could have
   * written just the following:
   *
   * def append(that: => LazyList[A]): LazyList[A]
   *
   * but this would not allow adding a list of doubles to a list of integers
   * (creating a list of numbers).  Compare this with the definition of
   * getOrElse last week, and the type of foldRight this week.
   */
  def append[B >: A](that: => LazyList[B]): LazyList[B] = 
    this.foldRight(that)((elem, acc) => cons(elem, acc))

  // Note: The type is incorrect, you need to fix it
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = 
    this.foldRight(Empty)((elem, acc) => f(elem).append(acc))

  // Exercise 9
  // Type answer here
  //
  // It is efficient as only the current element will be evaluated and then the
  // head will be given when the first element which lives up to the filter.
  // On normal lists filter will be applied to all elements and thus be linear.
  //
  // Scroll down to Exercise 10 in the companion object below

  // Exercise 13

  def mapUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this){
      case Cons(a, ax) => Some((f(a()), ax()))
      case Empty => None
    }

  def takeUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this, n)){
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(a, ax), n) => Some((a(), (ax(), n-1)))
    }

  def takeWhileUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this) {
      case Cons(a, ax) if p(a()) => Some((a(), ax()))
      case _ => None
    }

  def zipWith[B >: A, C](ope: (=> B, => B) => C)(bs: LazyList[B]): LazyList[C] =
    LazyList.unfold((this, bs)) {
      case (Cons(a, ax), Cons(b, bx)) => Some((ope(a(), b()), (ax(), bx())))
      case _ => None
    }

end LazyList // enum ADT



// The companion object for lazy lists ('static methods')

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))

  // Exercise 1

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  def to(n: Int): LazyList[Int] = cons(n, to(n-1))

  lazy val naturals: LazyList[Int] = from(1)

  // Scroll up to Exercise 2 to the enum LazyList definition 
  
  // Exercise 10

  // Note: The type is incorrect, you need to fix it
  lazy val fibs: LazyList[Int] =
    def aux(pprev: Int, prev: Int): LazyList[Int] =
      val cur = pprev + prev
      cons(pprev, aux(prev, cur))

    aux(0, 1)

  // Exercise 11

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): LazyList[A] = // Thanks atro for help
    (for
      (a, s) <- f(z)
    yield cons(a, unfold(s)(f))).getOrElse(Empty)

  // Exercise 12

  // Note: The type is incorrect, you need to fix it
  lazy val fibsUnfold: LazyList[Int] =
    unfold((0, 1))((pprev: Int, prev: Int) => Some(pprev, (prev, pprev + prev)))

  // Scroll up for Exercise 13 to the enum

end LazyList // companion object
