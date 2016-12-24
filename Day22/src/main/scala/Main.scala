import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day22-input.txt").getLines().toSeq.drop(2)
    val nodes = GridComputing.parseLines(lines)
    val answer1 = GridComputing.numViableNodes(nodes)
    println(s"answer1: $answer1")
  }
}
