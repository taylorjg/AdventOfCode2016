import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day23-input.txt").getLines.toSeq
    val program = Assembunny.parseProgram(lines)
    val answer1 = Assembunny.execute(program).a
    println(s"answer1: $answer1")
  }
}
