object Euler44 {
  def apply() = {
    // find the value of j+k that lower bounds the valid possibilities
    val z = Stream.from(1).filter(z => !allValidDiff(z).isEmpty).head

    // there's actually only one candidate, but for strict correctness, let's compute
    // all possible pents with z = j+k and return the one with min difference
    val possibilities = allValidDiff(z)
    val (j,k) = possibilities.sortBy{case (j,k) => pDiff(j,k)}.head
    println(s"P(j = ${j}) = ${pent(j)}")
    println(s"P(k = ${k}) = ${pent(k)}")
    pDiff(j,k)
  }

  //pn = n*(3n-1)/2
  var pents = (1 to 1000).map(pent).toArray
  private var maxPentIndex = 1000
  def morePents() {
    val lower = maxPentIndex + 1
    val upper = maxPentIndex*2
    pents = Array.concat(pents, (lower to upper).map(pent).toArray)
    maxPentIndex *= 2
  }

  def isPent(p: Int): Boolean = {
    if(pents(pents.length-1) < p) {
      println("Hmm, getting more pentagonal numbers")
      morePents()
      return isPent(p)
    }
    require(p <= pents(pents.length-1))
    java.util.Arrays.binarySearch(pents, p) > -1
  }

  def pent(n: Int): Int = n*(3*n-1)/2

  def pDiff(j: Int, k: Int): Int = {
    if (j > k) return pDiff(k,j)
    pent(k) - pent(j)
  }

  def valid(j: Int, k: Int): Boolean = {
    if (j > k) return valid(k,j)
    isPent(pent(k)-pent(j)) && isPent(pent(k) + pent(j))
  }

  // return all candidates with j+k = z
  // note that 2|pj - pk| = |3j^2 - 3k^2 + k - j| = 3(j+k)(j-k) - (j-k) = |j-k|[3(j+k) - 1] >= 3(j+k)-1
  // so that |pj-pk| >= [3(j+k)-1]/2 is monotonic in z
  // so all valid candidates with z' > z have greater abs of the diff
  def allValidDiff(z: Int) = {
    for {
      j <- 1 until z
      k = z - j
      if valid(j,k)
    } yield (j,k)
  }
}
