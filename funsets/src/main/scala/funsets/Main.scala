package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)

  val s = union(s1, s2)
  assert(contains(s, 1), "Union 1")
  assert(contains(s, 2), "Union 2")
  assert(!contains(s, 3), "Union 3")

  val s12: Set = union(s1, s2)
  val s23: Set = union(s2, s3)
  val sUnion = intersect(s12, s23)
  assert(!contains(sUnion, 1), "Intersect 1 should be false")
  assert(contains(sUnion, 2), "Intersect 2 should be true")
  assert(!contains(sUnion, 3), "Intersect 3 should be false")

  val sTDiff: Set = union(union(s1, s2),s3)
  val sDiff: Set = diff(sTDiff, s3)
  assert(contains(sDiff, 1), "Union 1")
  assert(contains(sDiff, 2), "Union 2")
  assert(!contains(sDiff, 3), "Union 3")

  val sTF: Set = union(union(union(s1, s2),s3),s4)
  def isPair(x: Int): Boolean = (x % 2 == 0)
  val sFilter: Set = filter(sTF, isPair)
  assert(!contains(sFilter, 1), "Pair filter 1")
  assert(contains(sFilter, 2), "Pair filter 2")
  assert(!contains(sFilter, 3), "Pair filter 3")
  assert(contains(sFilter, 4), "Pair filter 4")

  println("Done")
}
