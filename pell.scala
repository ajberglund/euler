object Pell{
  def euler66 = {
    // all pell solutions for non-square x between 2 and 1000
    val res = (2 to 1000).filter{x => !(1 until x).exists(s => s*s == x)}.map(Pell(_))
    // find the one with the largest value of n
    res.reduce{(a,b) => if (a.n > b.n) a else b}
  }

	//http://en.wikipedia.org/wiki/Chakravala_method !!
	def apply(p: Int) = {
		// starts slow for big p if you are far from solution
		// so bootstrap somewhere close
		// huge benefit for very large p
		// m ~ n/sqrt(p)
		val tmp = math.round(math.sqrt(p)).toInt
		val n0 = BigInt(tmp)//BigInt(1)
		val m0 = BigInt(1)
		val k0 = n0*n0 - m0*m0*p
		var t = Triplet(n0,m0,k0,p)
		while(!(t.k isEqualTo 1)){t = t.shrink}
		t
	}
	// houses a solution of n^2 - p m^2 = k
	case class Triplet(n: BigInt, m: BigInt, k: BigInt, p: Int){
		require(check, "Not a Pell Triplet")
		override def toString = s"${n}^2 - ${p}*${m}^2 = ${k}"
		// find another triplet with smaller value of k
		val absk = BigInt(k.i.abs())
		def shrink = {
			// find z s.t. n+m*z % k == 0 and
			// z minimizes z*z - N
			// for the second condition, there are two cases:
			// z < sqrt(N) and z > sqrt(N)
			// here, be brutish and check for the min of all values in case I (there are a finite number)
			// then start at maxZ from case I, and increment up (this monotonically increases z*z-N)
			// stop at the first value that is either less than I (keep it) or greater than I (keep I)

			var caseIBest: Option[(BigInt,BigInt)] = None // there may be no solution in branch I
			var z = BigInt(1)
			// exhaust case I
			while(z*z <= p){
				if ((n+z*m)%absk isEqualTo 0){
					val cur = abs(z*z - p)
					caseIBest = caseIBest.map{
						case (_,best) if (cur < best) => (z,cur) 
						case other => other
					}.orElse(Some((z,cur)))
				}
				z += 1
			}

			// check case II
			var done = false
			while(!done){
				if((n+z*m)%absk isEqualTo 0){
					val cur = abs(z*z-p)
					done = true
					z = caseIBest match {
						case Some((caseIz, caseIbest)) if (cur < caseIbest) => z
						case Some((caseIz, caseIbest)) if (cur >= caseIbest) => caseIz
						case _ => z
					}
				} else {z += 1}
			}
			var res = Triplet((n*z+m*p)/absk, (n+m*z)/absk, (z*z-p)/k, p)
			println(res)
			res
		}

		def abs(x: BigInt) = {if (x < 0) x*(-1) else x}

		def check = n*n - m*m*p isEqualTo k
	}

	import java.math.BigInteger
	case class BigInt(i: BigInteger) {
		override def toString = i.toString
		def +[T](that: T) = BigInt(i.add(BigInt(that.toString).i))
		def -[T](that: T) = BigInt(i.subtract(BigInt(that.toString).i))
		def *[T](that: T) = BigInt(i.multiply(BigInt(that.toString).i))
		def /[T](that: T) = BigInt(i.divide(BigInt(that.toString).i))
		def %[T](that: T) = BigInt(i.mod(BigInt(that.toString).i))
		def <[T](that: T) = i.compareTo(BigInt(that.toString).i) == -1
		def <=[T](that: T) = i.compareTo(BigInt(that.toString).i) <= 0
		def >=[T](that: T) = i.compareTo(BigInt(that.toString).i) >= 0
		def >[T](that: T) = i.compareTo(BigInt(that.toString).i) == 1
		def isEqualTo[T](that: T) = i.compareTo(BigInt(that.toString).i) == 0
	}

	object BigInt {
		// sloppy, will barf on a lot, but works for T = Int, Long etc
		def apply[T](x: T): BigInt = new BigInt(new BigInteger(x.toString))
	}

}

