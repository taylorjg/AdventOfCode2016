object FirewallRules {

  def parseLines(lines: Seq[String]): Seq[(Long, Long)] = {
    def parseLine(line: String): (Long, Long) =
      line match {
        case LineRegex(low, high) => (low.toLong, high.toLong)
      }
    lines map parseLine
  }

  private final val LineRegex = """(\d+)-(\d+)""".r

  def lowestValueNotBlocked(ranges: Seq[(Long, Long)], from: Long, to: Long): Long = {
    val sortedRanges = ranges sortBy { case (low, _) => low }
    sortedRanges foreach println
    @annotation.tailrec
    def loop(v: Long, ri: Int): Long = {
      val (l, h) = sortedRanges(ri)
      v match {
        case _ if v < l => v
        case _ if v <= h => loop(h + 1, ri + 1)
        case _ => loop(v, ri + 1)
      }
    }
    loop(from, 0)
  }
}
