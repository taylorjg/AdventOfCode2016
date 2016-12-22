import org.scalatest.FlatSpec

class RogueTests extends FlatSpec {

  "given example 1" should "return the correct 3 rows" in {
    val actual = Rogue.getRows("..^^.", 3)
    val expected =
      """
        |..^^.
        |.^^^^
        |^^..^
      """.stripMargin.split("\n") map (_.trim) filter (_.nonEmpty)
    assert(actual.toArray sameElements expected)
  }

  "given example 2" should "contain 38 safe tiles in the first 10 rows" in {
    assert(Rogue.getNumSafeTiles(".^^.^.^^^^", 10) == 38)
  }
}
