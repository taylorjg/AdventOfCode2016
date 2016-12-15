import org.scalatest.FlatSpec

class DecompressionTests extends FlatSpec {

  "example 1" should "decompress correctly" in {
    assert(Decompression.decompress("ADVENT") == "ADVENT")
  }

  "example 2" should "decompress correctly" in {
    assert(Decompression.decompress("A(1x5)BC") == "ABBBBBC")
  }

  "example 3" should "decompress correctly" in {
    assert(Decompression.decompress("(3x3)XYZ") == "XYZXYZXYZ")
  }

  "example 4" should "decompress correctly" in {
    assert(Decompression.decompress("A(2x2)BCD(2x2)EFG") == "ABCBCDEFEFG")
  }

  "example 5" should "decompress correctly" in {
    assert(Decompression.decompress("(6x1)(1x3)A") == "(1x3)A")
  }

  "example 6" should "decompress correctly" in {
    assert(Decompression.decompress("X(8x2)(3x3)ABCY") == "X(3x3)ABC(3x3)ABCY")
  }
}
