import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val instructions = Source.fromResource("day10-input.txt").getLines.toSeq
    val botGraph = BalanceBots.processInstructions(instructions)
    // BalanceBots.dumpBotGraph(botGraph)
    BalanceBots.dumpBotTree(botGraph)
    val answer = botGraph.findComparerOf(61, 17)
    println(s"answer: $answer")
  }
}
