import org.scalatest.FlatSpec

class DiscTimingTests extends FlatSpec {
  "given example" should "return first time of 5" in {
    val discs = Seq(
      Disc(1, 5, 4),
      Disc(2, 2, 1)
    )
    assert(DiscTiming.firstTime(discs) == 5)
  }
}
