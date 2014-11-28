//octave:29> factorial(9)*2 + 6*factorial(8) + 6*factorial(7) + 2*factorial(6) + 5*factorial(5) + factorial(4) + 2*factorial(3) +
//2*factorial(2)
//ans =  1000000
object Euler24 {
  def apply() = {
    val a = Array(0,1,2,3,4,5,6,7,8,9)
    var k = 1
    while(k < 1000000){
      k += 1
      next(a)
    }
    a.mkString("")
  }

  def next(a: Array[Int]) {
    // mutate array a to next "lexiographic" permutation
    // procedure:
    // starting from the right-most position n
    // n--
    // find k > n such that a(k) > a(n) and a(k) is minimized
    // if a(k) < a(n), swap positions k and n, then sort n+1 to end
    // else continue
    var n = a.length - 1
    var done = false
    while(!done){
      n -= 1;
      val t = a.slice(n+1,a.length)
      if (t.exists(_ > a(n))){
        val m = t.filter(_ > a(n)).min
        if (m > a(n)){
          val k = t.indexWhere(_ == m)
          a(k+n+1) = a(n) // (index of t) = (index of a) + n+1
          a(n) = m
          // sort the values after n
          a.view(n+1,a.length).sorted.copyToArray(a, n+1)
          done = true
        }
      }
    }
  }
}
