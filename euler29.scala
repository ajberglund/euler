//:load eratosthenes.scala
val primes = Sieve(1000)

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

//scala> result.toSet.size
//res15: Int = 9183

