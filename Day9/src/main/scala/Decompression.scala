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
      val openBracketPos = s.indexOf('(')
      val closeBracketPos = s.indexOf(')')
      if (openBracketPos >= 0 && closeBracketPos >= 0) {
        val prefix = s.substring(0, openBracketPos)
        val marker = s.substring(openBracketPos + 1, closeBracketPos)
        val m = MarkerRegex.findAllIn(marker)
        val length = m.group(1).toInt
        val count = m.group(2).toInt
        val data = s.substring(closeBracketPos + 1, closeBracketPos + 1 + length)
        val repeatedData = repeat(data, count)
        val suffix = s.substring(closeBracketPos + 1 + length)
        loop(suffix, acc + prefix + repeatedData)
      }
      else acc + s
    }
    loop(input, "")
  }

  def decompressedLength(input: String): Int = {
    @annotation.tailrec
    def loop(s: String, acc: Int): Int = {
      val openBracketPos = s.indexOf('(')
      val closeBracketPos = s.indexOf(')')
      if (openBracketPos >= 0 && closeBracketPos >= 0) {
        val marker = s.substring(openBracketPos + 1, closeBracketPos)
        val m = MarkerRegex.findAllIn(marker)
        val length = m.group(1).toInt
        val count = m.group(2).toInt
        val suffix = s.substring(closeBracketPos + 1 + length)
        loop(suffix, acc + openBracketPos + (length * count))
      }
      else acc + s.length
    }
    loop(input, 0)
  }

  def decompressedLengthV2(input: String): Long = {
    @annotation.tailrec
    def loop(s: String, acc: Long): Long = {
      val openBracketPos = s.indexOf('(')
      val closeBracketPos = s.indexOf(')')
      if (openBracketPos >= 0 && closeBracketPos >= 0) {
        val marker = s.substring(openBracketPos + 1, closeBracketPos)
        val m = MarkerRegex.findAllIn(marker)
        val length = m.group(1).toInt
        val count = m.group(2).toInt
        val data = s.substring(closeBracketPos + 1, closeBracketPos + 1 + length)
        val expandedDataLength = decompressedLengthV2(data)
        val suffix = s.substring(closeBracketPos + 1 + length)
        loop(suffix, acc + openBracketPos + (expandedDataLength * count))
      }
      else acc + s.length
    }
    loop(input, 0)
  }

  private final val MarkerRegex = """(\d+)x(\d+)""".r
}
