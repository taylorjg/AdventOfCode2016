object Decompression {

  def decompress(input: String): String = {
    def repeat(input: String, count: Int): String = {
      @annotation.tailrec
      def loop(s: String, n: Int, acc: String): String =
        if (n == 0) acc else loop(s, n - 1, acc + s)
      loop(input, count, "")
    }
    @annotation.tailrec
    def loop(s: String, acc: String): String = {
      val openBracket = s.indexOf('(')
      val closeBracket = s.indexOf(')')
      if (openBracket >= 0 && closeBracket >= 0) {
        val prefix = s.substring(0, openBracket)
        val marker = s.substring(openBracket + 1, closeBracket)
        val m = MarkerRegex.findAllIn(marker)
        val length = m.group(1).toInt
        val count = m.group(2).toInt
        val data = s.substring(closeBracket + 1, closeBracket + 1 + length)
        val repeatedData = repeat(data, count)
        val suffix = s.substring(closeBracket + 1 + length)
        loop(suffix, acc + prefix + repeatedData)
      }
      else acc + s
    }
    loop(input, "")
  }

  def decompressedLength(input: String): Int = {
    @annotation.tailrec
    def loop(s: String, acc: Int): Int = {
      val openBracket = s.indexOf('(')
      val closeBracket = s.indexOf(')')
      if (openBracket >= 0 && closeBracket >= 0) {
        val prefix = s.substring(0, openBracket)
        val marker = s.substring(openBracket + 1, closeBracket)
        val m = MarkerRegex.findAllIn(marker)
        val length = m.group(1).toInt
        val count = m.group(2).toInt
        val suffix = s.substring(closeBracket + 1 + length)
        loop(suffix, acc + prefix.length + (count * length))
      }
      else acc + s.length
    }
    loop(input, 0)
  }

  private final val MarkerRegex = """(\d+)x(\d+)""".r
}
