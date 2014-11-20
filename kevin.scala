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

object Factor {
  var primes = Sieve(100000).toSet

  def addPrimes(newPrimes: Set[Int]) {
    primes ++= newPrimes
  }

  def factorAndAdd(x: Int): Array[Int] = {
    val factors = factor(x)
    val fSet = factors.toSet
    if (!(fSet.diff(primes).isEmpty)){
      addPrimes(fSet)
    }
    factors
  }

  def factor(x: Int):Array[Int] = {
    // first attempt factorization using known primes
    // if x is a known prime
    if(primes.contains(x)){ return Array(x) } // x is prime

    // search for factors among the current known primes
    val knownPrimeFactors = primes.filter(x % _ == 0).toArray
    
    if (knownPrimeFactors.length != 0) {
      // we found some prime factors in our known set
      // divide these out and keep going
      val rem = knownPrimeFactors.foldLeft(x)(_ / _)
      if (rem == 1) {return knownPrimeFactors}
      return knownPrimeFactors ++ factor(knownPrimeFactors.foldLeft(x)(_ / _))
    }

    // it is not a known prime, and we haven't found any factors in our known set:
    // brute force it
    val fac = (2 to math.round(math.sqrt(x)).toInt).filter(x % _ == 0).headOption.getOrElse(x) 

    if (fac == x){ 
      // it has no divisors between 2 and sqrt(x), i.e. it is prime
      return Array(x)
    }else{
      val factors = Array(fac) ++ factor(x / fac)
      // we brute forced this, so add the new primes to the known primes
      return factors
    }
  }

  def isPaired(s: Array[Int]) = s.length % 2 == 0 && {val srt = s.sorted; val l = s.length; (0 until l/2).forall{ix => srt(2*ix) ==
      srt(2*ix+1)}}

  def main(args: Array[String]) {
    val p = args(0).toInt
    var x = 2
    if (args.length > 1){
      x = args(1).toInt
    }
    var happy = false
    var iter = 0
    while(!happy){
      iter +=1
      val f1 = factor(x)
      val f2 = factor(p*x + 2)
      val f3 = factor(p*x - 2)
      if(isPaired(f1 ++ f2) || isPaired(f1 ++ f2)){
        happy = true
        println(x)
        println(s"Found it! x = ${x}")
      }else{
        x += 1
      } 
      if(x % 100000 == 0){println(x)}
    }
  }
}
