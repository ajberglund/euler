object Euler32 {
  // rules for valid "pandigital" product
  // 10^(|a|+|b|-1) < a*b < 10^(|a|+|b|)
  // 10^(|c|-1) < c < 10^|c|
  // => two inequalities
  // 10^(a+b-1) < a*b=c < 10^c
  // 10^(c-1) < c = a*b < 10^(a+b)
  // => a+b-1 < c AND c -1 < a+b
  // but a+b+c=9, so these imply
  // 8 < 2*c < 11 OR
  // c = 4 or 5 !
  def apply() = {
    val pds = for{
      p <- (1 to 9).toArray.permutations
      bDiv <- (4 to 5)
      aDiv <- (1 to bDiv-1)
      pd <- check(p, aDiv, bDiv)
    } yield pd
    pds.map(_.c).toSet.sum
  }

  def check(p: Array[Int], aDiv: Int, bDiv: Int) = {
    // given array a, split it into a and b and divisors a,b 
    val a = p.slice(0, aDiv).mkString("").toInt
    val b = p.slice(aDiv, bDiv).mkString("").toInt
    val c = p.slice(bDiv, p.length).mkString("").toInt
    if (c == a*b) Some(Pandigital(a,b,c)) else None
  }

}

case class Pandigital(a: Int, b: Int, c: Int)
