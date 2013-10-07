package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }
  
  test("pascal: col=0,row=1") {
    assert(pascal(0,1) === 1)
  }
  
  test("pascal: col=1,row=1") {
    assert(pascal(1,1) === 1)
  }


  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("pascal: col=-1,row=-3") {
	  val ex = intercept[IllegalArgumentException] {
		  (pascal(-1,-3) === 0)
	  }
  	  assert(ex.getMessage() === "c and r must be positive")
  }
}
