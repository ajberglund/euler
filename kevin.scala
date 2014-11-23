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
  val primes = {
    println("Initializing primes")
    Sieve(100).map(_.toLong).toArray.sorted
  }

  def knownPrime(x: Long) = java.util.Arrays.binarySearch(primes, x) >= 0

  def factor(x: Long, startBruteAt: Option[Long] = None):Array[Long] = {
    val sq = math.sqrt(x)

    if(!startBruteAt.isDefined){
      // first attempt factorization using known primes
      // if x is a known prime
      //if(primes.contains(x)){ return Array(x) } // x is prime
      if(knownPrime(x)){ return Array(x) } // x is prime

      // search for factors among the current known primes
      val knownPrimeFactors: Array[Long] = {
        for {
          prime <- primes
          if prime <= sq && x % prime == 0
        } yield prime
      }
     
      if (knownPrimeFactors.length != 0) {
        // we found some prime factors in our known set
        // divide these out and keep going
        val rem = knownPrimeFactors.foldLeft(x)(_ / _)
        if (rem == 1) {return knownPrimeFactors}
        return knownPrimeFactors ++ factor(rem)
      }
    }

    // it is not a known prime, and we haven't found any factors in our known set:
    // brute force it, starting at max prime in our set
    val fac = {
      var b = -1l
      var candidate = startBruteAt.getOrElse(primes(primes.length-1))
      while (candidate <= sq && b < 0){
        if (x % candidate == 0) b = candidate
        candidate += 2 // skip even numbers
      }
      if (b < 0) x else b
    }

    if (fac == x){ 
      // it has no divisors between 2 and sqrt(x), i.e. it is prime
      return Array(x)
    }else{
      val factors = Array(fac) ++ factor(x / fac, Some(fac)) // jump right back into the brute loop from fac
      return factors
    }
  }

  def sqrtPerfectSquare(factors: Array[Long]) = {
    //returns the square root of x *under the assumption that factors are the prime factors of a perfect square!
    val f = factors.sorted
    (0 until f.length / 2).map(ix => f(2*ix))
  }

  def isPerfectSquare(x: Long, lower: Long, upper: Long): (Boolean, Long) = {
    if (x > 4*lower*lower) return isPerfectSquare(x, 2*lower, upper)
    if(x < (upper / 2)*(upper / 2)) return isPerfectSquare(x, lower, upper / 2)
    // sqrt(x) between lower and upper
    var candidate = lower
    while(candidate < upper && candidate*candidate <= x){
      if (candidate*candidate == x) return (true, candidate)
      candidate += 1
    }
    (false, -1l)
  }

  def isPerfectSquare(factors: Array[Long]) = desquare(factors).reduce(_*_) == 1

  def factorsFormValidX(p: Int, factors: Array[Long], neg: Boolean = false) = {
    // test whether x*(p*x + 2) is a perfect square, when x = prod(factors)
    // try to factorize as little as possible
    val term1 = factors.reduce(_*_)
    val term2 = if(neg) {p*term1 - 2} else {p*term1 + 2}

    val leftovers = desquare(factors)
    leftovers.forall(term2 % _ == 0) && {
      val term3 = leftovers.fold(term2)(_/_)
      isPerfectSquare(factor(term3))
    }
  }

  def desquare(s: Array[Long]): Array[Long] = {
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
    val numThreads = args(1).toInt
    var x = 2l
    if (args.length > 2){
      x = args(2).toLong
    }
    var happy = false
    println("Starting at x = " + x)
    var iter = 1
    val dist = (0 until numThreads).par
    while(!dist.map(d => check(x+d,p)).reduce(_||_)){
      x += numThreads
      iter += 1
      if(iter % (100000/numThreads) == 0){println(x)}
    }
  }
  def check(x: Long, p: Int) = {
      val f = factor(x) // this is where all the time is spent...
      // first check
      val plusWorks = factorsFormValidX(p, f, false)
      val minusWorks = factorsFormValidX(p, f, true)
      var happy = false

      val eq = s"n^2-${p}*m^2=1"
      var mString = ""
      var nString = ""

      if(plusWorks || minusWorks){
        happy = true
        println(x)
        println(s"Found it! x = ${x}")
        if(plusWorks){
          nString = s"n=${factor(p*x+1).mkString("*")}"
          mString = s"m=${sqrtPerfectSquare(factor(x) ++ factor(p*x+2)).mkString("*")}"
        } else {
          nString = s"n=${factor(p*x-1).mkString("*")}"
          mString = s"m=${sqrtPerfectSquare(factor(x) ++ factor(p*x-2)).mkString("*")}"
        }
        println(eq)
        println(nString)
        println(mString)
        import scala.sys.process._
        val cmd = {s"""echo "${nString}\n${mString}""""} #| {s"""mail -s "${eq}" berglund@quantifind.com"""}
        cmd.!
      }
      happy
  }
}
