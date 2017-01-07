import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day6-input.txt").getLines.toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)

    val answer1 = ErrorCorrection.correct(cleanedLines, useMostCommon = true)
    println(s"answer1: $answer1")

    val answer2 = ErrorCorrection.correct(cleanedLines, useMostCommon = false)
    println(s"answer2: $answer2")
  }
}
