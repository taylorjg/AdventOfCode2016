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
    assert(FirewallRules.lowestValueNotBlocked(ranges) == 3L)
  }

  "my part 2 example" should "have 15 non-blocked values" in {
    val input =
      """
        |9-11
        |4-6
        |15-19
      """.stripMargin
    // non-blocked values: 0 1 2 3 7 8 12 13 14 20 21 22 23 24 25
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val ranges = FirewallRules.parseLines(lines)
    assert(FirewallRules.numValuesNotBlocked(ranges, 25L) == 15L)
  }
}
