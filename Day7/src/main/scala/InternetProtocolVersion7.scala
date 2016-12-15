import scala.io.Source

object InternetProtocolVersion7 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day7-input.txt").getLines.toList
    val answer = lines map supportsTLS count (_ == true)
    println(s"answer: $answer")
  }

  def supportsTLS(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    supernets.exists(_.containsABBA) && hypernets.forall(!_.containsABBA)
  }

  def supportsSSL(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    ???
  }

  private def partitionInput(input: String): (List[String], List[String]) = {
    @annotation.tailrec
    def loop(s: String, ss: List[String], hs: List[String]): (List[String], List[String]) = {
      val openBracketPos = s.indexOf('[')
      val closeBracketPos = s.indexOf(']')
      if (openBracketPos > 0 && closeBracketPos > 0) {
        val supernet = s.substring(0, openBracketPos)
        val hypernet = s.substring(openBracketPos + 1, closeBracketPos)
        val rest = s.substring(closeBracketPos + 1)
        loop(rest, supernet :: ss, hypernet :: hs)
      }
      else {
        (s :: ss, hs)
      }
    }
    loop(input, List.empty, List.empty)
  }

  private implicit class StringOps(s: String) {
    def containsABBA: Boolean = {
      def isABBA(iv: String): Boolean = {
        val c1 = iv(0)
        val c2 = iv(1)
        val c3 = iv(2)
        val c4 = iv(3)
        c1 == c4 && c2 == c3 && c1 != c2
      }
      val possibleABBAs = s.indices dropRight 3 map (start => s.substring(start, start + 4))
      possibleABBAs exists isABBA
    }
  }
}
