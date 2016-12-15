import scala.io.Source

object InternetProtocolVersion7 {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day7-input.txt").getLines.toList

    val answer1 = lines map supportsTLS count (_ == true)
    println(s"number of IPs that support TLS: $answer1")

    val answer2 = lines map supportsSSL count (_ == true)
    println(s"number of IPs that support SSL: $answer2")
  }

  def supportsTLS(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    supernets.exists(_.containsABBA) && hypernets.forall(!_.containsABBA)
  }

  def supportsSSL(input: String): Boolean = {
    val (supernets, hypernets) = partitionInput(input)
    def correspondingBAB(aba: String): Boolean = hypernets exists (_.containsBAB(aba))
    val allABAs = supernets flatMap (_.findABAs)
    allABAs exists correspondingBAB
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

  private def isABBA(iv: String): Boolean = {
    val c1 = iv(0)
    val c2 = iv(1)
    val c3 = iv(2)
    val c4 = iv(3)
    c1 == c4 && c2 == c3 && c1 != c2
  }

  private def isABA(iii: String): Boolean = {
    val c1 = iii(0)
    val c2 = iii(1)
    val c3 = iii(2)
    c1 == c3 && c1 != c2
  }

  private def isBAB(iii: String, aba: String): Boolean = {
    val a = aba(0)
    val b = aba(1)
    val c1 = iii(0)
    val c2 = iii(1)
    val c3 = iii(2)
    c1 == b && c2 == a && c3 == b
  }

  private implicit class StringOps(s: String) {

    def containsABBA: Boolean = {
      val possibleABBAs = s.indices dropRight 3 map (start => s.substring(start, start + 4))
      possibleABBAs exists isABBA
    }

    def findABAs: Seq[String] = {
      val possibleABAs = s.indices dropRight 2 map (start => s.substring(start, start + 3))
      possibleABAs filter isABA
    }

    def containsBAB(aba: String): Boolean = {
      val possibleBABs = s.indices dropRight 2 map (start => s.substring(start, start + 3))
      possibleBABs exists (isBAB(_, aba))
    }
  }
}
