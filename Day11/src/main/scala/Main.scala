import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val arrangement = Source.fromResource("day11-input.txt").getLines.toSeq
  }
}
