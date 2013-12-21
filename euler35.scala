// This is brutish in a bunch of ways
// I didn't use the fact that a circular prime must be composed of digits 1,3,7,9
// Also didn't memoize circular primes, so as not to reevaluate them when seeded from
// another permutation
// BUT it has the cool feature of implicit conversions, so you can say
//scala> (1 to 1000000).filter(_.isCircularPrime).length
//res7: Int = 55

:load eratosthenese.scala

class IntDigit(a: Array[Int]) {
  override def toString() = {
    var str = ""
    a.reverse.foreach{str += _}
    str
  }
  val length = a.length

  // a(k) is the 10^k digit in some Int
  val int = {
    var base = 1
    var sum = 0
    a.foreach{ d => 
     sum += d*base
     base *= 10
    }
    sum
  }

  val isPrime = IntDigit.test(int)

  def isCircularPrime = {
    IntDigit.allRotations(a).forall(_.isPrime)
  }
}

implicit def intToIntDigit(n: Int): IntDigit = IntDigit(n)

object IntDigit {
  def apply(n: Int) = {
    // silly, but works
    new IntDigit(n.toString.toArray.reverse.map(_.toString.toInt))
  }
 val primes = Sieve(1000)

 def test(n: Int) = {
   // prime test, works for 1 <= n <= N^2, where pr = Sieve(N)
   n != 1 && !primes.filter(_ < n).exists(n % _ == 0)
  }

 def allRotations(a: Array[Int]): List[IntDigit] = {
   val l = a.length

   (0 until l).map{ rot => {
       val newA = a.clone()
       (0 until l).foreach{ idx =>
           newA((idx + rot) % l) = a(idx)
       }
       new IntDigit(newA)
     }
    }.toList
  }
}

