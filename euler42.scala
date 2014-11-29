object Euler42 {
  def apply() = {
    scala.io.Source.fromFile("words.txt").mkString.split(",")
      .filter(isTriangular).length
  }

  val alphabet = Array(
    '"','A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z').zip(0 to 26).toMap

  val triangularNumbers = (2 to 1000).scanLeft(1)(_+_).toArray

  def wordToNum(s: String) = s.toArray.map(alphabet(_)).sum

  // n = m*(m+1)/2 for some m?
  def isTriangular(n: Int): Boolean = {
    val k = triangularNumbers.length
    require(n <= triangularNumbers(k-1))
    java.util.Arrays.binarySearch(triangularNumbers, n) > -1
  }

  def isTriangular(s: String): Boolean = isTriangular(wordToNum(s))
}
