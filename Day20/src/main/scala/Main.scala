import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day20-input.txt").getLines.toSeq
    val ranges = FirewallRules.parseLines(lines)
    val answer1 = FirewallRules.lowestValueNotBlocked(ranges)
    println(s"answer1: $answer1")
  }
}
