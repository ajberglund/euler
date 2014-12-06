import com.ajberglund.euler.Util

object Euler29 {
  val primes = Util.primesPast(1000).map(_.toInt)

  def factorize(n: Int): List[Int] = {
    if (primes.contains(n)) List(n) 
    else {
      val p = primes.find(n % _ == 0).get
      p :: factorize(n / p)
    }.sorted
  }

  val N = 100
  val result = for {
    a <- (2 to N);
    f = factorize(a);
    b <- (2 to N)
  } yield (1 to b).flatMap(_ => f).toList.sorted

  def apply() = result.toSet.size

  //scala> result.toSet.size
  //res15: Int = 9183
}
