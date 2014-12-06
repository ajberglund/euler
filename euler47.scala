object Euler47 {
  def valid(n: Int, d: Int) = {
    (0 until d).forall(j => com.ajberglund.euler.Util.factor(n + j).distinct.size == d)
  }

  def solve(d: Int) = Stream.from(2).filter(n => valid(n, d)).head

  def apply() = solve(4)
}
