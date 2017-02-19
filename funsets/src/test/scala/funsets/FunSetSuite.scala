package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
   test("string take") {
     val message = "hello, world"
     assert(message.take(5) == "hello")
   }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
   test("adding ints") {
     assert(1 + 2 === 3)
   }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  test("Intersect only contains intersection") {
    new TestSets {
      val s12: Set = union(s1, s2)
      val s23: Set = union(s2, s3)
      val s: Set = intersect(s12, s23)
      assert(!contains(s, 1), "Intersect 1 should be false")
      assert(contains(s, 2), "Intersect 2 should be true")
      assert(!contains(s, 3), "Intersect 3 should be false")
    }
  }
  test("Diff does not contain last element") {
    new TestSets {
      val sTDiff: Set = union(union(s1, s2),s3)
      val sDiff: Set = diff(sTDiff, s3)
      assert(contains(sDiff, 1), "Union 1")
      assert(contains(sDiff, 2), "Union 2")
      assert(!contains(sDiff, 3), "Union 3")
    }
  }
  test("Filter with pair") {
    new TestSets {
      val sTF: Set = union(union(union(s1, s2),s3),s4)
      def isPair(x: Int): Boolean = (x % 2 == 0)
      val sFilter: Set = filter(sTF, isPair)
      assert(!contains(sFilter, 1), "Pair filter 1")
      assert(contains(sFilter, 2), "Pair filter 2")
      assert(!contains(sFilter, 3), "Pair filter 3")
      assert(contains(sFilter, 4), "Pair filter 4")
    }
  }

  test("For all on small set") {
    new TestSets {
      val s1000 = singletonSet(1000)
      val s1001 = singletonSet(1001)
      val sTotForAll: Set = union(union(union(union(union(s1, s2),s3),s4),s1000),s1001)
      val sPairForAll: Set = union(union(union(s1000, s2),s1001),s4)
      assert(forall(sTotForAll, x => x > 0), "Ints sup to 0")
      assert(!forall(sTotForAll, x => x > 2), "Ints sup to 2")
      assert(!forall(sTotForAll, x => x%2 == 0), "Ints pair")
      assert(forall(sPairForAll, x => x%2 == 0), "Ints pair except 1001")
    }
  }
  test("Exists on small set") {
    new TestSets {
      val s1000 = singletonSet(1000)
      val s1001 = singletonSet(1001)
      val sTotExists: Set = union(union(union(union(union(s1, s2),s3),s4),s1000),s1001)
      val sPairExists: Set = union(union(s1000, s2),s4)
      assert(exists(sTotExists, x => x == 2), "2 exists in set")
      assert(exists(sTotExists, x => x > 10), "Ints sup to 10")
      assert(!exists(sTotExists, x => x > 1000), "Ints sup to bound")
      assert(!exists(sPairExists, x => x%2 == 1), "Non pair in pair set")
    }
  }
  test("Map with pair, uneven") {
    new TestSets {
      val s: Set = union(union(union(s1, s2),s3),s4)
      def isPair(x: Int): Boolean = (x % 2 == 0)
      val sPair: Set = map(s, x => 2 * x)
      val sUneven: Set = map(s, x => 2 * x + 1)
      assert(forall(sPair, isPair), "All pair in x2 image")
      assert(!exists(sUneven, isPair), "No pair in 2x+1 image")
    }
  }

}
