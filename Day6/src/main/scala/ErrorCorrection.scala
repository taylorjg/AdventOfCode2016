import scala.io.Source

object ErrorCorrection {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day6-input.txt").getLines.toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    val answer = correct(cleanedLines)
    println(s"answer: $answer")
  }

  private final val ReverseOrder = Ordering[Int].reverse

  def correct(lines: List[String]): String = {
    def loop(numCols: Int, acc: String): String = {
      if (acc.length == numCols) acc
      else {
        val col = acc.length
        val sorted = (lines map (_ (col))).groupBy(identity).toList.sortBy(_._2.length)(ReverseOrder)
        val c = sorted.head._1
        loop(numCols, acc + c)
      }
    }
    loop(lines.head.length, "")
  }
}
