object Decompression {
  def decompress(input: String): String = {

    @annotation.tailrec
    def repeat(s: String, n: Int, acc: String): String =
      if (n == 0) acc else repeat(s, n - 1, acc + s)

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
        val repeatedData = repeat(data, count, "")
        val suffix = s.substring(closeBracket + 1 + length)
        loop(suffix, acc + prefix + repeatedData)
      }
      else acc + s
    }

    loop(input, "")
  }

  private final val MarkerRegex = """(\d+)x(\d+)""".r
}
