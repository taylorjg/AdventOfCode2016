import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromResource("day8-input.txt").getLines.toSeq
    val screen = Screen.processInstructions(50, 6, instructions)
    val answer = screen.matrix count identity
    println(s"answer: $answer")
  }
}
