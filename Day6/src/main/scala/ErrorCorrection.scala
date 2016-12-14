import scala.io.Source

object ErrorCorrection {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day6-input.txt").getLines.toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    val answer = correct(cleanedLines)
    println(s"answer: $answer")
  }

  def correct(lines: List[String]): String = {
    def loop(numCols: Int, acc: String): String = {
      if (acc.length == numCols) acc
      else {
        val col = acc.length
        val v1 = lines map (l => l(col))
        val v2 = v1.groupBy(identity).toList
        val v3 = v2.sortBy(kvp => -kvp._2.length)
        val v4 = v3.head._1
        val c = v4
        loop(numCols, acc + c)
      }
    }
    loop(lines.head.length, "")
  }
}
