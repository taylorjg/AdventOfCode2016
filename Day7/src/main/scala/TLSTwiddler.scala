object TLSTwiddler {

  def supportsTLS(s: String): Boolean = {
    val m = Regex.findAllIn(s)
    val bit1 = m.group(1)
    val bit2 = m.group(2)
    val bit3 = m.group(3)
    (bit1.containsABBA || bit3.containsABBA) && !bit2.containsABBA
  }

  private final val Regex = """(.*)\[(.*)\](.*)""".r

  private implicit class StringOps(s: String) {
    def containsABBA: Boolean = {
      def isABBA(iv: String): Boolean = {
        val c1 = iv(0)
        val c2 = iv(1)
        val c3 = iv(2)
        val c4 = iv(3)
        c1 == c4 && c2 == c3 && c1 != c2
      }
      s.indices take (s.length - 3) map (start => s.substring(start, start + 4)) exists isABBA
    }
  }
}
