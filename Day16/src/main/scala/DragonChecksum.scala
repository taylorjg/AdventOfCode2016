object DragonChecksum {

  def generateData(part1: String): String = {
    val part2 = part1
      .reverse
      .replace('0', 'X')
      .replace('1', '0')
      .replace('X', '1')
    Seq(part1, "0", part2).mkString
  }

  def generateChecksum(in: String): String = {
    @annotation.tailrec
    def loop(s: String): String = {
      def sameOrDifferent(cc: String): String = if (cc(0) == cc(1)) "1" else "0"
      val cs = s.grouped(2).map(sameOrDifferent).mkString
      if (cs.length % 2 == 0) loop(cs) else cs
    }
    loop(in)
  }

  def fillAndGenerateChecksum(length: Int, in: String): String = {
    @annotation.tailrec
    def fill(s: String): String = if (s.length >= length) s take length else fill(generateData(s))
    generateChecksum(fill(in))
  }
}
