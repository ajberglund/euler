object Euler23 {
  val abs = (12 to 28123).filter(isAbundant).toArray

  def isAbundant(x: Int) = {
    (1 to (x/2)).filter(x % _ == 0).reduce(_+_) > x
  }

  def isSumOfAbundant(x: Int) = {
    abs.exists(n => n < x && java.util.Arrays.binarySearch(abs, x - n) > -1)
  }

  def apply() = {
    (1 to 28123).filterNot(isSumOfAbundant).reduce(_+_)
  }
}
