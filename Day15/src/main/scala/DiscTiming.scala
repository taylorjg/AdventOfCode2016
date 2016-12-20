object DiscTiming {

  def firstTime(discs: Seq[Disc]): Int = {
    discs.length
  }

  def parseDiscs(lines: Seq[String]): Seq[Disc] = {
    def parseLine(line: String): Disc = {
      val m = LineRegex.findAllIn(line)
      val discNumber = m.group(1).toInt
      val numPositions = m.group(2).toInt
      val startingPosition = m.group(3).toInt
      Disc(discNumber, numPositions, startingPosition)
    }
    lines map parseLine
  }

  private final val LineRegex = """Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""".r
}
