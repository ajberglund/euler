object Euler38 {
  def concatenate(x: Int, n: Int) = (1 to n).map(_*x).mkString("")

  def panDigitals(n: Int) = {
    val result = collection.mutable.Buffer[String]()
    var x = 1
    var cand = "" 
    do{
      cand = concatenate(x, n)
      if (isPanDigital(cand)) result += cand
      x += 1
    } while (cand.length < 10)

    result
  }

  val pd = (1 to 9).toSeq
  def isPanDigital(s: String) = {
    s.split("").tail.map(_.toInt).toSeq.sorted == pd
  }

  def apply() = {
    (2 to 9).flatMap(panDigitals).max
  }
}
