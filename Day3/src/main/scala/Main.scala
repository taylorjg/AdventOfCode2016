import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day3-input.txt").getLines.toList

    val answer1 = BunnyTriangles.countValidTrianglesHorizontally(lines)
    println(s"Number of valid triangles (horizontally): $answer1")

    val answer2 = BunnyTriangles.countValidTrianglesVertically(lines)
    println(s"Number of valid triangles (vertically): $answer2")
  }
}
