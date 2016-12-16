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

  "example 1 v2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("ADVENT") == "ADVENT".length)
  }

  "example 2 v2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("A(1x5)BC") == "ABBBBBC".length)
  }

  "example 3 v2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("(3x3)XYZ") == "XYZXYZXYZ".length)
  }

  "example 4 v2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("A(2x2)BCD(2x2)EFG") == "ABCBCDEFEFG".length)
  }

  "example 5 v2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("(6x1)(1x3)A") == "(1x3)A".length)
  }

  "example 6 v2" should "decompress correctly" in {
    assert(Decompression.decompressedLength("X(8x2)(3x3)ABCY") == "X(3x3)ABC(3x3)ABCY".length)
  }
}
