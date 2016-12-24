import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day21-input.txt").getLines.toSeq
    val operations = Scrambling.parseLines(lines)
    val answer1 = Scrambling.scramble("abcdefgh", operations)
    println(s"answer1: $answer1")
  }
}
