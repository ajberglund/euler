package com.ajberglund.euler

object Util{
  val primes = collection.mutable.Buffer[Long](2l, 3l)
  def primesPast(n: Int) = {
    while(maxPrime < n){growPrimes()}
    primes
  }
  def maxPrime = primes(primes.length - 1)

  def growPrimes() {
    require(maxPrime > 2)
    var s = maxPrime + 2
    while(primes.exists(s % _ == 0)) s += 2
    primes += s
  }

  def factor(x: Long): Seq[Long] = {
    while(x > maxPrime*maxPrime){growPrimes()} // make sure we have enough primes, lazily

    val factors = primes.filter(x % _ == 0)

    if (factors.isEmpty) return Seq(x) // x is prime

    val red = factors.fold(x)(_ / _)
    if(red == 1) return factors // done

    factors.toSeq ++ factor(red) // factor the remainder
  }
}
