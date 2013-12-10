//Euler problem 15
//http://projecteuler.net/problem=15
//Compute number of ways to write integer N as sum of N+1 digits in (0 ... N)
//For example, in the 2x2 example, there are 6 paths:
//2 + 0 + 0
//1 + 1 + 0
//1 + 0 + 1
//0 + 1 + 1
//0 + 2 + 0
//0 + 0 + 2
// alternatively, imaging dropping N balls into N+1 urns
// how many different configurations can result?
// Denote this number by f(balls = N, urns = N+1)
// Note, f(0, N) = 1 // or is this 1?
// f(1, N) = N 
// After dropping n in first urn, 
// there are f(balls = N - n, urns = N ) remaining possibilities 
// so f(N, N+1) = f(N, N) + f(N - 1, N) + f(N - 2, N) + ... + f(0, N)

// hmmm. poor.
def f(balls: Long, urns: Long): Long = {
  if(urns < 1) 0
  else if (urns == 1) 1
  else if (balls == 1) urns
  else (0 until (balls + 1)).fold(0)((s,v) => s + f(v, urns - 1))
}

// still poor
def f2(n: Long, m: Long): Long = {
  if(n == 1) 1
  else if (m == 1) 1
  else f2(n - 1, m) + f2(n, m - 1)
}

//this was lame, cause it took a couple of minutes
//scala> f2(21,21)
//res6: Long = 137846528820
