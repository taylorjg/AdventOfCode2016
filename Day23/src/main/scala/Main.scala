import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines1 = Source.fromResource("day23-input.txt").getLines.toSeq
    val program1 = Assembunny.parseProgram(lines1)
    val answer1 = Assembunny.execute(program1, Register("a", 7)).a
    println(s"answer1: $answer1")

    val lines2 = Source.fromResource("day23-input-with-mul.txt").getLines.toSeq
    val program2 = Assembunny.parseProgram(lines2)
    val answer2 = Assembunny.execute(program2, Register("a", 12)).a
    println(s"answer2: $answer2")
  }
}
