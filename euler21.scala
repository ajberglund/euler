def divisors(n: Int) = (1 to n / 2).filter(n % _ == 0)

def sumDiv(n: Int) = divisors(n).sum

val m = (1 to 10000).map{idx => idx -> sumDiv(idx)}.toMap

val amicable = m.map{
  case (i, s) => (i, s, sumDiv(s))
}.filter{ 
  x => x._1 == x._3 && x._1 != x._2 //amicable, not perfect
}

amicable.foreach{a => println("+ " + a._1)}
println("---------------")
println("= " + amicable.map(_._1).sum)

