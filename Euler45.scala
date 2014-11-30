object Euler45 {
  // hexagonal numbers grow fastest, so increment these
  // for each one, increment Tn and Pn until they are large than Hn
  // then increment Hn, check equality of Tn and Pn
  def apply() = {
    var Tn = 285l
    var Pn = 165l
    var Hn = 143l + 1
    def T = Tn*(Tn+1)/2
    def P = Pn*(3*Pn-1)/2
    def H = Hn*(2*Hn - 1)

    while (H != T || H != P){
      Hn += 1
      while (H > T) Tn += 1
      while (H > P) Pn += 1
    }
    println(s"T(n=${Tn}) = ${T}")
    println(s"P(n=${Pn}) = ${P}")
    println(s"H(n=${Hn}) = ${H}")
  }
}

