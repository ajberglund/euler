// script: paste into REPL then solve()
//:load util.scala

// get all the 4 digit primes
val pr = Util.primesPast(9999).collect{case p if (p > 999 && p < 10000) => p}.map(_.toInt).toArray

// get the digits of an integer
def digits(n: Int) = {
  val b = collection.mutable.Buffer[Int]()
  var cur = n
  while (cur > 0){ b += cur % 10; cur = cur / 10 }
  b
}

def solve() = {
  // check prime pairs for existince of a third prime in the sequence
  // given p1 and p2 > p1, check whether p2 + (p2 - p1) = 2*p2-p1 is also a (4-digit) prime
  val candidates = 
    for{
      j <- 0 until pr.length
      p1 = pr(j); 
      k <- 1+java.util.Arrays.binarySearch(pr, p1) until pr.length 
      p2 = pr(k)
      if (java.util.Arrays.binarySearch(pr, 2*p2-p1) > -1)
    } yield (p1,p2,2*p2-p1)

  // filter to triplets with all the same digits
  candidates.filter{case (p1,p2,p3) => digits(p1).sorted == digits(p2).sorted && digits(p2).sorted == digits(p3).sorted}
}
