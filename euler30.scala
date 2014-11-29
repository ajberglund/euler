object Euler30 {
  // let c = ck 10^k + ... + c0 have k+1 digits
  // note largest possible sum of fifth powers of digits is if all ck = 9
  // (k+1) * 9^5
  // smallest (k+1) digit number is 10^(k+2)-1
  // so no need to check k with 10^(k+2) - 1 > (k+1)*9^5
  // crossover is at k = 6, i.e. all candidates < 10^6
  def satisfies(x: Int) = {
    val digits = x.toString.split("").tail
    digits.map(i => intPow(i.toInt, 5)).reduce(_ + _) == x
  }

  def intPow(n: Int, k: Int): Int = {
    if (n == 0 && k == 0) return 0
    if (k == 0) return 1
    else return n*intPow(n, k-1)
  }

  def apply() = {
    (2 until intPow(10,6)).filter(satisfies).sum
  }
}
