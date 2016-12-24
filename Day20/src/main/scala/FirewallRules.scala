object FirewallRules {

  type Range = (Long, Long)
  type Ranges = Seq[Range]

  def parseLines(lines: Seq[String]): Ranges = {
    def parseLine(line: String): Range =
      line match {
        case LineRegex(low, high) => (low.toLong, high.toLong)
      }

    lines map parseLine
  }

  private final val LineRegex = """(\d+)-(\d+)""".r

  def lowestValueNotBlocked(ranges: Ranges): Long = {
    val sortedRanges = ranges sortBy { case (low, _) => low }

    @annotation.tailrec
    def loop(v: Long, ri: Int): Long = {
      val (l, h) = sortedRanges(ri)
      v match {
        case _ if v < l => v
        case _ if v <= h => loop(h + 1, ri + 1)
        case _ => loop(v, ri + 1)
      }
    }

    loop(0L, 0)
  }

  def numValuesNotBlocked(ranges: Ranges, to: Long): Long = {

    val sortedRanges1 = ranges sortBy { case (low, _) => low }
    val sortedRanges2 = removeFullyOverlappedRanges(sortedRanges1)
    val sortedRanges3 = adjustPartiallyOverlappedRanges(sortedRanges2)

    @annotation.tailrec
    def loop(v: Long, ri: Int, acc: Long): Long = {
      if (sortedRanges3.indices.isDefinedAt(ri)) {
        val (l, h) = sortedRanges3(ri)
        v match {
          case _ if v < l => loop(h + 1, ri + 1, acc + l - v)
          case _ if v <= h => loop(h + 1, ri + 1, acc)
        }
      }
      else {
        acc + to - v + 1
      }
    }

    loop(0L, 0, 0L)
  }

  private def removeFullyOverlappedRanges(ranges: Ranges): Ranges = {
    def isFullyOverlapped(range: Range): Boolean = {
      val (low, high) = range
      ranges exists (r => {
        val (l, h) = r
        r != range && l <= low && h >= high
      })
    }

    ranges flatMap (range => if (isFullyOverlapped(range)) Seq() else Seq(range))
  }

  private def adjustPartiallyOverlappedRanges(ranges: Ranges): Ranges = {
    def adjustRangeIfNecessary(tuple: (Range, Int)): (Range, Int) = {
      val ((low1, high1), idx) = tuple
      if (ranges.isDefinedAt(idx + 1)) {
        val (low2, _) = ranges(idx + 1)
        if (low2 < high1) {
          ((low1, low2 - 1), idx)
        }
        else tuple
      }
      else tuple
    }

    ranges
      .zipWithIndex
      .map(adjustRangeIfNecessary)
      .map(_._1)
  }
}
