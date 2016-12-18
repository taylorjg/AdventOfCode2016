import org.scalatest.FlatSpec

class OneTimePadTests extends FlatSpec {
  "given example" should "find the 64th key at index 22728" in {
    assert(OneTimePad.findIndex("abc") == 22728)
  }
}
