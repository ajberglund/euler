def nextCollatz(z: Long) = if ((z % 2) == 0) z / 2 else 3 * z + 1

def collatzLength(z: Long): Long = {
  if (z == 1) 1
  else 1 + collatzLength(nextCollatz(z))
}

val (len, startVal) = 
  (1 to 1000000).foldLeft((1L,1L)){
    (x,y) => {
      val l = collatzLength(y) 
      if (l > x._1) (l,y) else x
    }
  }

println("Longest Collatz sequence length is " + len + ", starting from " + startVal)

