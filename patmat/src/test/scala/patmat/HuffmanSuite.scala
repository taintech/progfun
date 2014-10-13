package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val l1 = List('a', 'b', 'a')
    val freq1 = List(('a', 2), ('b', 1))
    val ls1 = List(Leaf('b', 1), Leaf('a', 2))
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val dummyText = "xtxextx"
    val dummyCodeTree = Fork(Leaf('x', 4), Fork(Leaf('t', 2), Leaf('e', 1), List('e', 't'), 3), List('e', 't', 'x'), 7)
    val dummyCodeTable: CodeTable = List(('e', List(1,1)), ('t', List(1,0)), ('x', List(0)))
    val dummySecret = List(0, 1, 0, 0, 1, 1, 0, 1, 0)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    new TestTrees {
      assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('t', 2), Leaf('e', 1), List('e', 't'), 3)))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times computes each unique character in the list") {
    new TestTrees {
      assert(times(l1).sorted === freq1)
    }
  }

  test("creates ordered leaf list") {
    new TestTrees {
      assert(makeOrderedLeafList(freq1) === ls1)
    }
  }

  test("checks whether the list `trees` contains only one single code tree") {
    new TestTrees {
      assert(singleton(List(Leaf('c', 0))))
      assert(!singleton(ls1))
    }
  }

  test("combine until it is singleton") {
    new TestTrees {
      assert(until(singleton, combine)(leaflist) === dummyCodeTree)
    }
  }

  test("creates code tree for chars") {
    new TestTrees {
      assert(createCodeTree(string2Chars(dummyText)) === dummyCodeTree)
    }
  }

  test("decode text") {
    new TestTrees {
      assert(decode(dummyCodeTree, dummySecret).mkString === "xtxext")
    }
  }

  test("encode text") {
    new TestTrees {
      assert(encode(dummyCodeTree)(string2Chars("xtxext")) === dummySecret)
      assert(encode(frenchCode)(decodedSecret) === secret)
    }
  }

  test("gets code bits") {
    assert(codeBits(List(('c', List(1,0,1))))('c') === List(1,0,1))
  }

  test("creates code table") {
    new TestTrees {
      assert(convert(dummyCodeTree) === dummyCodeTable)
    }
  }

  test("quick encode text") {
    new TestTrees {
      assert(quickEncode(dummyCodeTree)(string2Chars("xtxext")) === dummySecret)
      assert(quickEncode(frenchCode)(decodedSecret) === secret)
    }
  }
}
