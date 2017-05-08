package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  test("weight of an even larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times for abaacc char list") {
    assert(times(string2Chars("abaacc")).toSet === Set(('b',1), ('c',2), ('a',3)))
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('t',2), Leaf('e',1),List('t','e'),3), Leaf('x',4)))
  }
  test("combine of some leaf list with insert") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4),Leaf('x', 6),Leaf('y', 7))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('t',3), Leaf('e',2),List('t','e'),5), Leaf('x',6), Leaf('y',7)))
  }

  test("codeTree for abacca") {
    val ct = createCodeTree(string2Chars("abacca"))
    print(ct)
    assert(ct == Fork(Leaf('a',3), Fork(Leaf('c',2),Leaf('b',1),List('c', 'b'),3), List('a', 'c', 'b'),6))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  test("decode for simple") {
    val ct = createCodeTree(string2Chars("abacca"))
    assert(decode(ct,List(0,1,1,1,0)) == "abc".toList)
  }
}
