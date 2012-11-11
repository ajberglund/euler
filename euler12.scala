def sieve(N: Int): List[Int] = {
	// sieve of aritosthenes, return all primes less than or equal to N
	
	val primes = collection.mutable.ListBuffer[Int]()
	val candidates = (2 until N+1).toBuffer

	while(candidates.size >0){
		val prime = candidates(0)
		primes += prime
		candidates --= candidates.filter(_ % prime == 0)
	}

	primes.toList

}

def factors(n: Int, P: List[Int]) = {	
	var f = P.filter(n % _ == 0)

	var prod = f.reduce(_*_)

	while(n != prod){
		f ++= P.filter((n/prod) % _ ==0)
		prod = f.reduce(_*_)
	}

	f

}

def divisorsOfTn(n: Int, P: List[Int]): Int = {
	val (x,y): (Int, Int) = if(n % 2 == 0) (n/2, n+1) else (n, (n+1)/2)

	val f = factors(x, P) ++ factors(y, P)

	// hacky to get unique elements of list f as a list
	f.toSet.toList.map{x: Int => f.filter(_==x).size + 1}.reduce(_*_)

}

val maxN = 20000
val P = sieve(maxN) // get a bunch of primes
var n = 3

var max = 3
var cur = 3
while(cur < 500 && n < maxN){
	cur = divisorsOfTn(n, P)
	if(cur > max){
		println("n = " + n + ", divisors = " + cur)
		max = cur
	}
	n += 1 
}

n -= 1

println("Smallest triangle number with >500 divisors is " + (n*(n+1)/2).toString)
