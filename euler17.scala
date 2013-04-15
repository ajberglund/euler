// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

val ones = List("","one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
val tens = List("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
val hundreds = "" :: ones.tail.map(_ + "hundred")
val thousands = "" :: ones.tail.map(_ + "thousand")

val teenSub = Map("tenone" -> "eleven", "tentwo" -> "twelve", "tenthree"->"thirteen", "tenfour"->"fourteen", "tenfive"->"fifteen", "tensix"->"sixteen", "tenseven"->"seventeen",
	"teneight"->"eighteen", "tennine"->"nineteen")

val wordMap = Array(ones, tens, hundreds, thousands)

def intToWord(i: Int): String = {
var word: String = i.toString.toCharArray.reverse.zipWithIndex.map{case (coeff, power) => wordMap(power)(coeff.toString.toInt)}.reverse.reduce(_+_)
	teenSub.foreach{
		case (k,v) => word = word.replaceAll(k, v)
	}
	word.toString	
}

println((1 until 1001).map(intToWord(_).length + 3).sum - 3*(99+10)) // added 3 to each for "and" but removed it from 99+10 (all numbers under 100 and all multiples of 100)