object Euler31 {
  def numWays(amt: Int, coins: List[Int]): Int = {
    if (amt == 0) 1
    else if (amt < 0  || coins.isEmpty) 0
    else numWays(amt - coins.head, coins) + numWays(amt, coins.tail)
  }

  def apply() = numWays(200, List(1, 2, 5, 10, 20, 50, 100, 200))
}
