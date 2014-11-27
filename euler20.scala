//Project Euler 20
object Euler20 {
  // lame
  def apply() = (1 to 100).map(scala.math.BigInt(_)).reduce(_*_).toString.split("").tail.map(_.toInt).reduce(_+_) 

  def intDivideProd(a: Int, b: Int, k: Int) = {
    // return (a*b) / k without forming a*b
    k*(a/k) * (b/k) + (a/k)*(b % k) + (b/k)*(a % k) + ((a % k) * (b % k)) / k
  }

  def intDivideProd(x: Seq[Int], k: Int): Int = {
    if (x.size == 1) return x(0) / k
    else if (x.size == 2) return intDivideProd(x(0), x(1), k)
    else {
      val a = x.head
      val b = x.tail
      val bmodk = b.map(_ % k).reduce((t1,t2) => (t1*t2) % k)
      return k*(a/k) * intDivideProd(b, k) + (a/k)*bmodk + intDivideProd(b, k)*(a % k) + ((a % k) * (bmodk)) / k
    }
  }
}
