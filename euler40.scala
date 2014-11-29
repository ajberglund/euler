object Euler40 {
  def apply() = {
    (0 to 6).map(pow(10,_)).map(digit).reduce(_*_)
  }

  // returns the position of the last digit resulting from 
  // componentns of length n
  // e.g. lastDigit(1) = 9, since the ninth digit is 9
  // lastDigit(2) = 189 = 1*9 + 2*90 from the 90 2-digit numbers
  // etc
  def lastDigit(n: Int): Int = (1 to n).map(k => 9*k*pow(10,k-1)).sum

  // integer power
  def pow(base: Int, exp: Int): Int = {if (exp == 0) return 1 else return base * pow(base, exp-1)}

  // what is the kth digit?
  // compute this by first finding the largest n such that lastDigit(n-1) < k
  // then we know we are k - lastDigit(n) digits into the n-digit numbers
  // so the number is k/n and it is the k%n digit of that number
  def digit(k: Int) = {
    val n = Stream.from(1).filter(lastDigit(_) > k).head
    // now we know we're in the z = (k - lastdigit(n-1)) digit among numbers of length n
    // so we must be in the (z%n digit) of the (z/n)th n-digit number
    val z = k-lastDigit(n-1) - 1
    val num = pow(10,n-1) + (z/n)
    val res = (num.toString.split("").tail)(z%n).toInt
    println(s"${k}th digit is ${res} from ${num}")
    res
  }
}
