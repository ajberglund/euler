object Euler26 {
  def apply() = {
    val candidates = (2 to 1000)
    candidates.zip(candidates.map(period)).sortBy(-1.0 * _._2).head._1
  }
  // remove all factors of 2 and 5, then observe that the cycle period of z is the 
  // smallest d such that
  // 10^d mod(z) = 1
  def period(x: Int): Int = {
    if(x == 2 || x == 5){
      return 0
    } else if (x % 2 == 0) { 
      return period(x / 2) 
    } else if (x % 5 == 0) { 
      return period(x / 5) 
    } else {
      var d = 1 
      var m = scala.math.BigInt(10)
      while(!(m % x == 1)){
        d+=1
        m *= 10
      }
      return d
    }
  }
}

