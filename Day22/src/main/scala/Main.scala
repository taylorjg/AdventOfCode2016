import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day22-input.txt").getLines().toSeq
    val nodes = GridComputing.parseLines(lines)
    val answer1 = GridComputing.viableNodes(nodes).length
    println(s"answer1: $answer1")
    GridComputing.visualise(nodes)
  }
}
