object Euler41 {
  def isPrime(x: Int) = {
    !((2 until x/2).exists(x % _ == 0))
  }

  def largest(n: Int) = 
    (for {
      d <- (1 to n).toArray.permutations
      num = d.foldLeft(0)(10*_ + _)
      if isPrime(num)
    } yield num)

  def apply() = {
    (1 to 9).flatMap(largest).maxBy(x => x)
  }
}
