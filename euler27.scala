//:load eratosthenes.scala
// brutish
val primes = Sieve(1000)
def seqLength(a: Int, b: Int): Int = {
  def f(n: Int) = n * n + a* n + b
  var n = 0 
  while(primes.contains(f(n))) n += 1
  n
}

val seqLengths = for{
  b <- (0 to 999)
  a <- (-b to 999)
} yield ((a,b) -> seqLength(a, b))

//scala> seqLengths.maxBy(_._2)
//res1: ((Int, Int), Int) = ((-61,971),62)
