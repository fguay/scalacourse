package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if( c < 0 || r < 0 ){
      throw new IllegalArgumentException("c and r must be positive")
    }
    if(c == 0 || r == 0 || r == c) 1
    else (pascal(c, r-1) + pascal(c-1, r-1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parse(acc : Int, chars : List[Char]) : Boolean = {
    	if(chars.isEmpty) {
    	  acc == 0
    	} 
    	else if(chars.head == '(') parse(acc + 1, chars.tail)
        else if(chars.head == ')'){
    	  if(acc == 0) false else parse(acc - 1, chars.tail)
    	} else  parse(acc , chars.tail)
    	
    }
    parse(0, chars)  
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	    def countChangeIndex(index:Int, money : Int, coins: List[Int]) : Int = {
		    if(money == 0)  1
		    else if (money < 0 || coins.size == index) 0
		    else countChangeIndex(index, money-coins(index), coins) + countChangeIndex(index +1, money, coins)   
	  	}
	  	countChangeIndex(0, money, coins)
	}
}
