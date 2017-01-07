import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val steps = Bunny.parseSteps(Source.fromResource("day1-input.txt").mkString)
    val (answer1, answer2) = Bunny.distance(steps)
    println(s"answer1: $answer1")
    println(s"answer2: $answer2")
  }
}
