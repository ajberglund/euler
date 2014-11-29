object Euler36 {
  def apply() = {for {
    n <- 1 to 1000000
    if isPalindrome(toBinary(n)) && isPalindrome(n.toString.split("").tail.map(_.toInt))
  } yield n}.sum

  def toBinary(x: Int): Array[Int] = {
    var z = x
    if (x <= 1) return Array(x)
    else return toBinary(x / 2) ++ toBinary(x % 2)
  }

  def isPalindrome(a: Array[Int]) = {
    val l = a.length
    (0 to l/2-1).forall(k => a(k) == a(l - k - 1))
  }
}
