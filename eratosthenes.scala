package com.ajberglund.euler

import collection.mutable.Buffer

// Sieve of eratosthenes
object Sieve {
  def apply(N: Int): Array[Int] = {
    val primes = collection.mutable.Buffer[Int]()
    // in this array, k is prime if candidates(k-2) is true
    // initially, all elements are candidate primes
    val maybeNewPrime = Array.fill(N)(true)
    maybeNewPrime(0) = false
    maybeNewPrime(1) = false

    var p = 2

    while (p < N){
      primes += p

      // eliminate p and all its multiples from future contention
      var strike = p
      while(strike < N) {
        maybeNewPrime(strike) = false
        strike += p
      }
      // now get the first remaining candidate
      while (p < N && !maybeNewPrime(p)){p += 1}
    }
    primes.toArray
  }
}

