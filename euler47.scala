// :load util.scala in REPL
// the paste this
def valid(n: Int, d: Int) = {
  (0 until d).forall(j => Util.factor(n + j).distinct.size == d)
}

def solve(d: Int) = Stream.from(2).filter(n => valid(n, d)).head
