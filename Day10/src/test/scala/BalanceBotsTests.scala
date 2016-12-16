import org.scalatest.FlatSpec

class BalanceBotsTests extends FlatSpec {

  "given example" should "find the correct bot" in {
    val input =
      """
        |value 5 goes to bot 2
        |bot 2 gives low to bot 1 and high to bot 0
        |value 3 goes to bot 1
        |bot 1 gives low to output 1 and high to bot 0
        |bot 0 gives low to output 2 and high to output 0
        |value 2 goes to bot 2
      """.stripMargin
    val instructions = input.toInstructions
    val botGraph = BalanceBots.processInstructions(instructions)
    assert(botGraph.findComparerOf(5, 2).contains(2))
  }

  private implicit class StringOps(s: String) {
    def toInstructions: Seq[String] =
      s.split("\n") map (_.trim) filter (_.nonEmpty)
  }
}
