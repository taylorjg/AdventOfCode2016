import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day24-input.txt").getLines.toVector
    val airDuctSpelunking = new AirDuctSpelunking(lines)
    val answer1 = airDuctSpelunking.shortestRoute()
    println(s"answer1: $answer1")
  }
}
