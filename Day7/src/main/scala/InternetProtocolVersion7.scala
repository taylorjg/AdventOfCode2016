object InternetProtocolVersion7 {

  def supportsTLS(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    (supernets exists containsABBA) && (hypernets forall doesNotContainABBA)
  }

  def supportsSSL(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    def correspondingBAB(aba: String): Boolean = hypernets exists containsBAB(aba)
    val allABAs = supernets flatMap findABAs
    allABAs exists correspondingBAB
  }

  private def partitionInput(input: String): (List[String], List[String]) = {
    @annotation.tailrec
    def loop(s: String, ss: List[String], hs: List[String]): (List[String], List[String]) = {
      val openBracketPos = s.indexOf('[')
      val closeBracketPos = s.indexOf(']')
      if (openBracketPos >= 0 && closeBracketPos >= 0) {
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

  private def containsABBA(s: String): Boolean = {
    val possibleABBAs = possibilities(s, 4)
    possibleABBAs exists isABBA
  }

  private def doesNotContainABBA(s: String): Boolean = !containsABBA(s)

  private def findABAs(s: String): Seq[String] = {
    val possibleABAs = possibilities(s, 3)
    possibleABAs filter isABA
  }

  private def containsBAB(aba: String)(s: String): Boolean = {
    val possibleBABs = possibilities(s, 3)
    possibleBABs exists isBAB(aba)
  }

  private def possibilities(s: String, len: Int): Seq[String] =
    s.indices dropRight (len - 1) map (start => s.substring(start, start + len))

  private def isABBA(s: String): Boolean = {
    val c1 = s(0)
    val c2 = s(1)
    val c3 = s(2)
    val c4 = s(3)
    c1 == c4 && c2 == c3 && c1 != c2
  }

  private def isABA(s: String): Boolean = {
    val c1 = s(0)
    val c2 = s(1)
    val c3 = s(2)
    c1 == c3 && c1 != c2
  }

  private def isBAB(aba: String)(s: String): Boolean = {
    val a = aba(0)
    val b = aba(1)
    val c1 = s(0)
    val c2 = s(1)
    val c3 = s(2)
    c1 == b && c2 == a && c3 == b
  }
}
