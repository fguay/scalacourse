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
  
  test("singleton") {
  new TestTrees {
    assert( !singleton(Leaf('a',2) :: Leaf('b',3) :: Nil ))
    assert( singleton(Leaf('b',3) :: Nil ))
    
    
  }
}

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
      assert(timesCount('c', List(('a',1),('c',3))) === List(('a',1),('c',4)))
      assert(timesCount('c', List(('a',1),('b',3))) === List(('a',1),('b',3),('c',1)))
      assert(timesCount('c', List()) === List(('c',1)))
      assert(times(List('a','b','c','b','a')) === List(('a',2),('b',2),('c',1)))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
    test("makeOrderedLeafList for some  table") {
    assert(makeOrderedLeafList(times("to be or not to be".toList)) === List(Leaf('r',1), Leaf('n',1), Leaf('b',2), Leaf('e',2), Leaf('t',3), Leaf('o',4), Leaf(' ',5)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("decode t1 & t2") {
    new TestTrees {      
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("ab".toList)) === "ab".toList)
      }
  }
  
  test("convert codetable") {
    new TestTrees {      
    	assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
      }
  }
  
  test("createcodetree") {
    new TestTrees {      
        val charslist = string2Chars("to be or not to be")
        assert(createCodeTree(charslist) === Fork(Fork(Leaf('o',4),Fork(Leaf('b',2),Leaf('e',2),List('b', 'e'),4),List('o', 'b', 'e'),8),Fork(Leaf(' ',5),Fork(Fork(Leaf('r',1),Leaf('n',1),List('r', 'n'),2),Leaf('t',3),List('r', 'n', 't'),5),List(' ', 'r', 'n', 't'),10),List('o', 'b', 'e',' ' , 'r', 'n', 't'),18))
    	assert(encode(createCodeTree(charslist))(charslist).length===47)
      }
  }
  
  test("decode frenchcode") {
    new TestTrees {      
      assert(decodedSecret === "huffmanestcool".toList)
      assert(decode(Huffman.frenchCode, encode(Huffman.frenchCode)("frenchcode".toList)) === "frenchcode".toList)
      assert(decode(Huffman.frenchCode, quickEncode(Huffman.frenchCode)("frenchcode".toList)) === "frenchcode".toList)
      }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(quickEncode(t1)("ab".toList) === encode(t1)("ab".toList))
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
