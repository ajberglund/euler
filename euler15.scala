//Euler problem 15
//http://projecteuler.net/problem=15
//Basic recurrence is
//paths(n, m) = paths(n-1, m) + paths(n, m-1)
//with 
//paths(1, m) = 1
//paths(n, 1) = 1
//Solve recurrence with dynamic program (i.e. cache/'memoize' the results)
//actually, i don't know if this is technically DP, but it works like a charm!

object LatticePaths {
  private val cache = collection.mutable.Map[(Int, Int), Long]()
  private def fCache(n: Int, m: Int): Long = cache.getOrElseUpdate((n,m), f(n,m))

  private def f(n: Int, m: Int): Long = {
     if(n == 1 || m == 1) 1
     else fCache(n - 1, m) + fCache(n, m - 1)
  }

  def apply(n: Int) = f(n + 1, n + 1)
}

//scala> LatticePaths(2)
//res9: Long = 6
//
//scala> LatticePaths(20)
//res10: Long = 137846528820 // instantaneous!

