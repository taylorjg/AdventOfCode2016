object BunnyTriangles {

  private def isValidTriangle(numbers: Seq[Int]): Boolean = {
    val sortedNumbers = numbers.sorted
    sortedNumbers.take(2).sum > sortedNumbers.last
  }

  private def parseLine(s: String): Seq[Int] =
    s.split("""\s""") map (_.trim) filter (_.nonEmpty) map (_.toInt)

  def countValidTrianglesHorizontally(lines: Seq[String]): Int = {
    def isValidLine(line: String): Boolean = isValidTriangle(parseLine(line))
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    cleanedLines count isValidLine
  }

  def countValidTrianglesVertically(lines: Seq[String]): Int = {
    def extractCol(parsedLines: Seq[Seq[Int]])(col: Int): Seq[Int] = parsedLines map (_(col))
    def countValidTrianglesInGroupOfLines(groupOfLines: Seq[String]): Int = {
      val parsedLines = groupOfLines map parseLine
      val cols = parsedLines.head.indices
      val transposedLines = cols map extractCol(parsedLines)
      transposedLines count isValidTriangle
    }
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    val groupedLines = cleanedLines grouped 3
    groupedLines.foldLeft(0)((acc, groupOfLines) => acc + countValidTrianglesInGroupOfLines(groupOfLines))
  }
}
