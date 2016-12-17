import org.scalatest.FlatSpec

class FredTests extends FlatSpec {
  "given example" should "report the correct number of moves" in {
    val input =
      """
        |The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
        |The second floor contains a hydrogen generator.
        |The third floor contains a lithium generator.
        |The fourth floor contains nothing relevant.
      """.stripMargin
    val arrangement = input.split("\n") map (_.trim) filter (_.nonEmpty)
    assert(Fred.process(arrangement) == 11)
  }
}
