import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val instructions = Source.fromResource("day10-input.txt").getLines.toSeq
    val botGraph = BalanceBots.processInstructions(instructions)

    val answer1 = botGraph.findComparerOf(61, 17)
    println(s"answer1: $answer1")

    val answer2 = for {
      a <- botGraph.getOutputValue(0)
      b <- botGraph.getOutputValue(1)
      c <- botGraph.getOutputValue(2)
    } yield a * b * c
    println(s"answer2: $answer2")
  }
}
