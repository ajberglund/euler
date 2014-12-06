package com.ajberglund.euler

import collection.mutable.Buffer

// Sieve of eratosthenes
object Sieve {
  def apply(N: Int): Array[Int] = {
    val primes = collection.mutable.Buffer[Int]()

    // initially, all elements are candidate primes, so maybeNewPrime(p) = true for all p
    val maybeNewPrime = Array.fill(N)(true)

    // initialize
    maybeNewPrime(0) = false
    maybeNewPrime(1) = false

    // start sieve here
    var p = 2

    // while any candidate primes remain
    while (p < N){
      primes += p

      // eliminate p and all its multiples from future contention
      // i.e. set maybeNewPrime to false
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

