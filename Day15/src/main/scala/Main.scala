import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day15-input.txt").getLines.toSeq
    val discs = DiscTiming.parseDiscs(lines)
    val answer1 = DiscTiming.firstTime(discs)
    println(s"answer1: $answer1")
  }
}
