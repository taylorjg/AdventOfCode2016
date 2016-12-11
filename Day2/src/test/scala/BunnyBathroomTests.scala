import org.scalatest.FlatSpec

class BunnyBathroomTests extends FlatSpec {

  private final val Instructions =
    """
      |ULL
      |RRDDD
      |LURDL
      |UUUUD
    """.stripMargin

  private final val Lines = Instructions.split("\n")

  "example with normal keypad" should "return a bathroom code of 1985" in {
    assert(BunnyBathroom.figureOutTheCode(NormalKeyPad)(Lines) == "1985")
  }

  "example with extended keypad" should "return a bathroom code of 5DB3" in {
    assert(BunnyBathroom.figureOutTheCode(ExtendedKeyPad)(Lines) == "5DB3")
  }
}
