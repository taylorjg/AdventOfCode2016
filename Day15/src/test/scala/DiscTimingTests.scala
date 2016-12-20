import org.scalatest.FlatSpec

class DiscTimingTests extends FlatSpec {
  "given example" should "return first time of 5" in {
    val input =
      """
        |Disc #1 has 5 positions; at time=0, it is at position 4.
        |Disc #2 has 2 positions; at time=0, it is at position 1.
      """.stripMargin
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val discs = DiscTiming.parseDiscs(lines)
    assert(DiscTiming.firstTime(discs) == 5)
  }
}
