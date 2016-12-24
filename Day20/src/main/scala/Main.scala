import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day20-input.txt").getLines.toSeq
    val ranges = FirewallRules.parseLines(lines)

    val answer1 = FirewallRules.lowestValueNotBlocked(ranges)
    println(s"answer1: $answer1")

    val answer2 = FirewallRules.numValuesNotBlocked(ranges, 4294967295L)
    println(s"answer2: $answer2")
  }
}
