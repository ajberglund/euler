object Euler37 {
  def apply() = {
    var d = 2
    val truncatables = collection.mutable.Buffer[Int]()
    while(truncatables.length < 11){
      truncatables ++= curious(d)
      d += 1
    }
    truncatables
  }

  def isCurious(a: Seq[Int]) = {
    //val a = p.toString.split("").tail.map(_.toInt)
    val subSequences = (1 to a.length).flatMap(k => Seq(a.takeRight(k), a.take(k))).distinct
    subSequences.forall(a => isPrime(a.foldLeft(0)(10*_ + _)))
  }

  def isPrime(x: Int) = {
    x > 1 && !(2 to x-1).exists(x % _ == 0)
  }

  def curious(d: Int) = {
    val a = Array(1,2,3,5,7,9)
    for{
      digits <- (1 to d).flatMap(_ => a).combinations(d)
      candidate <- digits.permutations
      if isCurious(candidate)
    } yield candidate.foldLeft(0)(10*_ + _)
  }
}
