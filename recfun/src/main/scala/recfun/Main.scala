package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balancing braces")
    println(balance("(just an) example".toList))
    println(balance("())(".toList))
    println(balance("(hey there))".toList))
    println(balance("(hey there)(".toList))
    println(balance("())((hey there)".toList))
    println(balance("(hey (there)".toList))

    println("Counting chage:")
    print("5 - [1,2]: ")
    println(countChange(5, List(1,2)))
    print("5 - [2,1]: ")
    println(countChange(5, List(2,1)))
    print("10 - [2,4,3]: ")
    println(countChange(10, List(2,4,3)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def openParentheses(charArr: List[Char], openToDate: Int): Int = {
        if (charArr.isEmpty)
          openToDate
        else if (openToDate < 0)
          openParentheses(charArr.tail, -1)
        else if (charArr.head == "(".charAt(0))
          openParentheses(charArr.tail, openToDate + 1)
        else if (charArr.head == ")".charAt(0))
          openParentheses(charArr.tail, openToDate - 1)
        else
          openParentheses(charArr.tail, openToDate)
      }
      openParentheses(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0)
        0
      else if (money == 0)
        1
      else
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
