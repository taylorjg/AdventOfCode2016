import org.scalatest.FlatSpec

class FirewallRulesTests extends FlatSpec {
  "the given example" should "have a lowest non-blocked value of 3" in {
    val input =
      """
        |5-8
        |0-2
        |4-7
      """.stripMargin
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val ranges = FirewallRules.parseLines(lines)
    assert(FirewallRules.lowestValueNotBlocked(ranges) == 3)
  }
}
