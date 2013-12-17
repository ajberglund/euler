// Sieve of eratosthenes
object Sieve {
  def apply(N: Int): List[Int] = {
    val (pr, ca) = sift(List(), (2 to N).toList)
    pr.reverse
  }

  import annotation.tailrec
  @tailrec
  private def sift(primes: List[Int], candidates: List[Int]): (List[Int], List[Int]) = {
   if (candidates.isEmpty) (primes, List())
   else {
     val p = candidates.head
     sift(p :: primes, candidates.filter(_ % p != 0))
   }
  }
}

