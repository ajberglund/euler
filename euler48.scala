object Euler48 {
  // don't keep any numbers larger than 10^10
  def apply() = {
    (1 to 1000).map(n => Euler48.powMod(n,n,10000000000l)).reduce((a,b) => (a + b) % 10000000000l)
  }

  def powMod(base: Long, exp: Long, k: Long): Long = {
    // return (base^exp)modk
    // using (n*n) mod k = [(n mod k) * (n mod k)] mod k
    if (exp == 0) return 1
    else return ((base % k) * powMod(base, exp-1, k)) % k
  }
}
