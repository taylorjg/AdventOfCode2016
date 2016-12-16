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

  "example 1 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("ADVENT") == "ADVENT".length)
  }

  "example 2 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("A(1x5)BC") == "ABBBBBC".length)
  }

  "example 3 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("(3x3)XYZ") == "XYZXYZXYZ".length)
  }

  "example 4 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("A(2x2)BCD(2x2)EFG") == "ABCBCDEFEFG".length)
  }

  "example 5 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("(6x1)(1x3)A") == "(1x3)A".length)
  }

  "example 6 length" should "decompress to the correct length" in {
    assert(Decompression.decompressedLength("X(8x2)(3x3)ABCY") == "X(3x3)ABC(3x3)ABCY".length)
  }

  "v2 example 1" should "decompress to the correct length" in {
    assert(Decompression.decompressedLengthV2("(3x3)XYZ") == "XYZXYZXYZ".length)
  }

  "v2 example 2" should "decompress to the correct length" in {
    assert(Decompression.decompressedLengthV2("X(8x2)(3x3)ABCY") == "XABCABCABCABCABCABCY".length)
  }

  "v2 example 3" should "decompress to the correct length" in {
    assert(Decompression.decompressedLengthV2("(27x12)(20x12)(13x14)(7x10)(1x12)A") == 241920)
  }

  "v2 example 4" should "decompress to the correct length" in {
    assert(Decompression.decompressedLengthV2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") == 445)
  }
}
