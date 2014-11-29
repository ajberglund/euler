object Euler43 {
  def apply() = {
    for{
      d <- (0 to 9).toArray.permutations
      if d.slice(1,4) % 2 == 0
      if d.slice(2,5) % 3 == 0
      if d.slice(3,6) % 5 == 0
      if d.slice(4,7) % 7 == 0
      if d.slice(5,8) % 11 == 0
      if d.slice(6,9) % 13 == 0
      if d.slice(7,10) % 17 == 0
    } yield arrayToLong(d)
  }

  implicit def arrayToLong(a: Array[Int]): Long = a.foldLeft(0l)(10*_+_)
}
