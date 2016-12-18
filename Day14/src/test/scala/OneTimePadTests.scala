import org.scalatest.FlatSpec
import HashType._

class OneTimePadTests extends FlatSpec {

  "given example with normal hashing" should "find the 64th key at index 22728" in {
    assert(OneTimePad.findIndex("abc", Normal) == 22728)
  }

  "given example with stretched hashing" should "find the 64th key at index 22551" in {
    assert(OneTimePad.findIndex("abc", Stretched) == 22551)
  }
}
