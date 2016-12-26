import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day25-input.txt").getLines.toSeq
    val program = Assembunny.parseProgram(lines)

    val desiredOutput = List.fill(50)(List(0, 1)).flatten
    println(s"desiredOutput (first 100 values): $desiredOutput")

    @annotation.tailrec
    def loop(n: Int): Int = {
      val output = Assembunny.execute(program, Register("a", n)).output.reverse
      if (output == desiredOutput) n else loop(n + 1)
    }

    val answer1 = loop(0)
    println(s"answer1: $answer1")
  }
}
