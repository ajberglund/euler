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
  var primes = Sieve(100).toSet // alg is fastest when this set is in some sweet spot, not too large, not too small

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

  def sqrtPerfectSquare(factors: Array[Int]) = {
    //returns the square root of x *under the assumption that factors are the prime factors of a perfect square!
    val f = factors.sorted
    (0 until f.length / 2).map(ix => f(2*ix))
  }

  def isPerfectSquare(factors: Array[Int]) = desquare(factors).reduce(_*_) == 1

  def factorsFormValidX(p: Int, factors: Array[Int], neg: Boolean = false) = {
    // test whether x*(p*x + 2) is a perfect square, when x = prod(factors)
    // try to factorize as little as possible
    val term1 = factors.reduce(_*_)
    val term2 = if(neg) {p*term1 - 2} else {p*term1 + 2}

    val leftovers = desquare(factors)
    leftovers.forall(term2 % _ == 0) && isPerfectSquare(factor(leftovers.fold(term2)(_ / _)))
  }

  def desquare(s: Array[Int]): Array[Int] = {
    // drop factors that occur in pairs, i.e. fctor out all perfect squares
    // e.g. Array(1,2,2,2,2,3,4,4,5) -> Array(1,3,5)
    if (s.length == 1) {return s}
    else if (s.length == 2) { 
      if (s(0) == s(1)) return Array(1) 
      else return s
    }
    else {
      val (h,t) = s.sorted.splitAt(1)
      if(h.head == t.head) {return desquare(t.tail)}
      else { return h ++ desquare(t) }
    }
  }

  def main(args: Array[String]) {
    val p = args(0).toInt
    var x = 2
    if (args.length > 1){
      x = args(1).toInt
    }
    var happy = false
    var iter = 0
    while(!happy){
      val f = factor(x) // this is where all the time is spent...
      // first check
      val plusWorks = factorsFormValidX(p, f, false)//(plus % desquaredProd == 0  && desquare(factor(plus / desquaredProd)).reduce(_*_)==1) 
      val minusWorks = factorsFormValidX(p, f, true)//(minu % desquaredProd == 0 && desquare(factor(minu / desquaredProd)).reduce(_*_)==1)

      if(plusWorks || minusWorks){
        happy = true
        println(x)
        println(s"Found it! x = ${x}")
        if(plusWorks){
          println(s"n = ${factor(p*x+1).mkString("*")}")
          println(s"m = ${sqrtPerfectSquare(factor(x) ++ factor(p*x+2)).mkString("*")}")
        } else {
          println(s"n = ${factor(p*x-1).mkString("*")}")
          println(s"m = ${sqrtPerfectSquare(factor(x) ++ factor(p*x-2)).mkString("*")}")
        }
        println(s"solves n^2 - ${p}*m^2 = 1")
      }else{
        x += 1
      } 
      if(x % 100000 == 0){println(x)}
    }
  }
}
