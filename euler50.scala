// Euler 50
// find the longest consecutive prime sequence adding to p
// NOTE: there can be at most one sub-sequence of length k that sums to a given value
// consider the sliding window of length k on the primes. since the primes are non-decreasing, moving the window monotonically increases
// the sum
// k is bounded where the sum of first k primes is > N = 1000000
// this number is 546 for N = 1000000
// start from here, search each k for a prime, step down
object Euler50 {
  val (primes, primeSet) = {
    println("Initializing prime bitset")
    val pr = com.ajberglund.euler.Sieve(1000000)
    val ps = collection.immutable.BitSet(pr:_*)
    println("Done initializing!")
    (pr, ps)
  }

  def maxK(N: Int) = {
    var (ix, sum) = (-1,0)
    while(sum < N){
      ix += 1
      sum += primes(ix)
    }

    ix
  }

  def kPrimeSeq(k: Int, N: Int): Option[Int] = {
    // check whether there exists a sequence of k primes whose sum is prime
    // and less than N
    var (ix, sum) = (-1,0)

    // initialize to the sum of first k primes
    while(ix < k-1){
      ix += 1
      sum += primes(ix)
    }
    
    // now slide the k-window along the primes until the sum exceeds N
    while (sum < N && !primeSet(sum)){
      // at every point, sum = primes.slice(ix-k+1, ix+1)
      ix += 1
      sum += primes(ix)
      sum -= primes(ix - k)
    }

    val found = primeSet(sum)
    if (found) println(s"${sum} = ${primes.slice(ix-k, ix).mkString("+")}")

    if (found) Some(sum) else None
  }

  def apply(N: Int) = {
    var k = maxK(N)
    var seq = kPrimeSeq(k, N)
    while(!seq.isDefined){
      k -= 1
      seq = kPrimeSeq(k, N)
    }
    seq
  }
}
