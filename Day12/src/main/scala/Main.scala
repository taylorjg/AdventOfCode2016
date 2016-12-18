import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val code = Source.fromResource("day12-input.txt").getLines.toVector

    val answer1 = Assembunny.execute(code).map("a")
    println(s"answer1: $answer1")

    val answer2 = Assembunny.execute(code, Register("c", 1)).map("a")
    println(s"answer2: $answer2")
  }
}
