/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Prop
import org.scalactic.TripleEquals.*


object ExamSpec
  extends org.scalacheck.Properties("exam-2024-autumn"):

  property("A test that always passes (a sanity check)") =
    forAll { (n: Int) => n == n }

end ExamSpec

// vim:cc=66
