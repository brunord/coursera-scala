package recfun

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
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(str: List[Char], count: Int): Boolean = {
      if(str.isEmpty) if(count == 0) true else false
      else if(str.head == ')'){ if(count == 0) false else loop(str.tail, count-1) }
      else if(str.head == '(') loop(str.tail, count +1)
      else loop(str.tail,count)
    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty || money <= 0){
        0
      } else if(coins.size == 1){
        if(money%coins.head==0) 1 else 0
      }else{
        if(coins.head > money) {
          loop(money, coins.tail)
        }else if(coins.head == money){
          1 + loop(money, coins.tail)
        }else{
          loop(money-coins.head, coins) + loop(money, coins.tail)
        }
      }
    }

    loop(money, coins.distinct)
  }
}
