package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  
  trait Level3 extends SolutionChecker {
      /* terrain for level 1*/

   val level =
      """------ooooooo--
        |oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin

  }
  
  trait Level6 extends SolutionChecker {
      /* terrain for level 1*/

   val level = 
    """-------oooooo--
      |-----o--ooo----
      |-----o--ooooo--
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo----
      |------ooooo----
      |------ooooo----
      |-------ooo-----""".stripMargin

  }
  
  
  test("terrain string parser") {
    new Level1 {
      val vec = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
      assert(terrainFunction(vec)(Pos(0,0)))
      assert(!terrainFunction(vec)(Pos(10,10)))
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
    }
  }
  
  test("startBlock") {
    new Level1 {
      assert(startBlock.b1 == Pos(1,1) && startBlock.b2 == Pos(1,1))
    }
  }
  
  test("is Legal ans isStanding") {
    new Level1 {
      assert(startBlock.isLegal)
      assert(startBlock.isStanding)
      
      assert(!new Block(Pos(1,1), Pos(1,2)).isStanding)
      assert(!new Block(Pos(100,100), Pos(200,200)).isLegal)
    }
  }
  
  test("legalNeighbors") {
    new Level1 {
      assert(startBlock.neighbors.size === 4)
      assert(startBlock.legalNeighbors.size === 2)
     }
  }
  
 test("done") {
    new Level1 {
      assert(done( new Block(Pos(goal.x, goal.y), Pos(goal.x, goal.y)) ))
    }
  } 
 
 test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(startBlock, List(Left, Up)).toSet === Set(
		  (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
		  (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
    }
  } 
 
 test("newNeighborsOnly") {
    new Level1 {
      assert(newNeighborsOnly(
        Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),(Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream,
    	Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) ===
    	Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)  
    }
  } 

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }
  
  test("optimal solution length for level 3") {
    new Level3 {
      assert(solution.length === 19)
    }
  }
  
   test("optimal solution length for level 6") {
    new Level6 {
      assert(solution.length === 0)
    }
  }
}
