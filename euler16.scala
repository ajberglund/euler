// Euler 16
class CappedInt {
  override def toString = {
    digits.slice(0, numDigits).toList.reverse.map(_.toString).reduce(_ + _)
  }
  val N = 1000
  val digits = Array.ofDim[Int](N)

  // initialize array to value 1
  var numDigits = 1
  digits(0) = 1

  def *= (n: Int) {
    var i = 0
    var prevR = 0 // remainder to carry

    while(i < numDigits){
        val newI = n * digits(i)
        val newR = newI / 10
        digits(i) = newI - 10 * newR + prevR

        if (i == numDigits - 1 && newR > 0) numDigits += 1
        prevR = newR
        i += 1
      }
  }
}

// solution to Euler 16:
val i = new CappedInt // value = 1
(0 until 1000).foreach{_ => i *= 2}
println(i.digits.sum)

