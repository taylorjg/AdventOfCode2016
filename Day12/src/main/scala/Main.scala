import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val code = Source.fromResource("day12-input.txt").getLines.toVector
    val registers = Assembunny.execute(code)
    val answer = registers.map("a")
    println(s"answer: $answer")
  }
}
