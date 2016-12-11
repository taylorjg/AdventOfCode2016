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
    def extractCol(nsh: Seq[Seq[Int]], col: Int): Seq[Int] = nsh map (_(col))
    def countValidTrianglesInGroupOf3Lines(groupOfLines: Seq[String]): Int = {
      val nsh = groupOfLines map parseLine
      val nsv = 0 to 2 map (col => extractCol(nsh, col))
      nsv count isValidTriangle
    }
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    val groupedLines = cleanedLines.grouped(3)
    groupedLines.foldLeft(0)((acc, groupOfLines) => acc + countValidTrianglesInGroupOf3Lines(groupOfLines))
  }
}
