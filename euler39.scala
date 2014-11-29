object Euler39 {
  def apply(upTo: Int = 1000) = {
    val q = (1 to upTo/2).maxBy{q => solutions(2*q).length }
    2*q
  }

  // a^2+b^2 = (p-a-b)^2 = p^2 -2*p(a+b)+(a+b)^2
  // 0 = p^2 - 2p(a+b) + 2ab
  def solutions(p: Int) = {
    for{
      a <- 1 until p/2
      n = p*(p-2*a)
      d = 2*(p-a)
      if n % d == 0
      b = n / d
      if b >= a //WLOG b >= a
    } yield (a, b, p - a - b)
  }
}
