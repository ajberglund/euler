object Euler46 {
  def apply() = {
    // go up in decades
    var dec = 1000
    var res: Option[Int] = None
    while(res.isEmpty){
      println(s"Checking golbach numbers <= ${dec}")
      val gbs = goldbachs(dec)
      val oddComposites = (2 to dec).filter(_ % 2 == 1).filterNot(primes.contains(_))
      val violators = oddComposites.diff(gbs)
      if (violators.size > 0) res = Some(violators.sorted.head)
      dec *= 10
    }
    res.get
  }

  val primes = collection.mutable.Buffer[Int](2)
  def nextPrime = {
    val maxPrime = primes(primes.length - 1)
    val nextPrime = Stream.from(maxPrime + 1).filter(p => !primes.exists(p % _ == 0)).head
    primes += nextPrime
    nextPrime
  }

  def goldbachSequence(prime: Int, upper: Int) = {
    // return all the goldbachs anchored on prime, and less than or equal to upper
    val b = collection.mutable.Buffer[Int]()
    var sq = 1
    var cur = prime + 2*sq*sq
    while (cur <= upper){
      b += cur
      sq += 1
      cur = prime + 2*sq*sq
    }
    b.filterNot(primes.toSet)// primes are not goldbachs
  }

  def goldbachs(upper: Int) = {
    // all golbach numbers less than or equal to upper
    // computed from all sequences with p <= upper
    while(primes(primes.length-1) <= upper){nextPrime}
    // goldbachs must have p \neq 2
    primes.drop(1).flatMap(prime => goldbachSequence(prime, upper)).distinct
  }
}
