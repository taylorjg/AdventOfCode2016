import scala.io.Source

object ErrorCorrection {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day6-input.txt").getLines.toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)

    val answer1 = correct(cleanedLines, useMostCommon = true)
    println(s"answer using most common letters: $answer1")

    val answer2 = correct(cleanedLines, useMostCommon = false)
    println(s"answer using most common letters: $answer2")
  }

  private final val Ascending = Ordering[Int]
  private final val Descending = Ordering[Int].reverse

  def correct(lines: List[String], useMostCommon: Boolean): String = {
    val ordering = if (useMostCommon) Descending else Ascending
    def charOfInterest(col: Int): Char = {
      val sorted = (lines map (_ (col))).groupBy(identity).toList.sortBy(_._2.length)(ordering)
      sorted.head._1
    }
    def loop(numCols: Int, acc: String): String = {
      if (acc.length == numCols) acc
      else {
        val col = acc.length
        val c = charOfInterest(col)
        loop(numCols, acc + c)
      }
    }
    loop(lines.head.length, "")
  }
}
