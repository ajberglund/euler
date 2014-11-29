object Euler34 {
  // c! <= 9! = 362880
  // d-digit number has sumFactor(n) <= d* (9!)
  // and d-digit number has n >= 10^d
  // so 10^d <= n = sumFactor(n) <= d*9!
  // => d <= 6
  def apply() = 
  for{
    n <- (3 to 1000000)
    if isSumOfFactorials(n)
  } yield n

  def isSumOfFactorials(x: Int) = {
    x.toString.split("").tail.map(n => factorial(n.toInt)).sum == x
  }

  def factorial(x: Int): Int = {
    if (x == 0) return 1
    else return x*factorial(x-1)
  }
}
