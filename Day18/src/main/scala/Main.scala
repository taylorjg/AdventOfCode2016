import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val firstRow = Source.fromResource("day18-input.txt").getLines().next()

    val rows = Rogue.getRows(firstRow, 40)
    val flattenedRows = rows.flatten
    val answer1 = flattenedRows count (_ == '.')
    println(s"answer1: $answer1")

    val answer2 = Rogue.getNumSafeTiles(firstRow, 400000)
    println(s"answer2: $answer2")
  }
}
