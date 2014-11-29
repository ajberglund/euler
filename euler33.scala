object Euler33 {
  // assuming, per Euler33, that num and den are length 2 arrays, containing the digits
  // of numeriator and denominator
  def apply() = 
    for{
      n1 <- 1 to 9
      n2 <- 1 to 9
      d1 <- n1 to 9
      d2 <- 1 to 9
      num = Array(n1,n2)
      den = Array(d1,d2)
      if den > num
      f = Fraction(Array(n1,n2), Array(d1,d2))
      if f.cancels
  } yield f

  case class Fraction(num:Array[Int], den:Array[Int]) {
    override def toString = s"${num(0)}${num(1)}/${den(0)}${den(1)}"
    def cancels = {
      (num(0) == den(0) && num(1)*den == den(1)*num) ||
      (num(1) == den(0) && num(0)*den == den(1)*num) ||
      (num(0) == den(1) && num(1)*den == den(0)*num) ||
      (num(1) == den(1) && num(0)*den == den(0)*num)
    }
  }

  implicit def arrayToInt(a: Array[Int]): Int = a(0)*10 + a(1)
}

