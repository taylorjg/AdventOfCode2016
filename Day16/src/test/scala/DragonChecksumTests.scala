import org.scalatest.WordSpec

class DragonChecksumTests extends WordSpec {

  "random data generation" can {
    "starting from 1" should {
      "generate 100" in {
        assert(DragonChecksum.generateData("1") == "100")
      }
    }
    "starting from 0" should {
      "generate 001" in {
        assert(DragonChecksum.generateData("0") == "001")
      }
    }
    "starting from 11111" should {
      "generate 11111000000" in {
        assert(DragonChecksum.generateData("11111") == "11111000000")
      }
    }
    "starting from 111100001010" should {
      "generate 1111000010100101011110000" in {
        assert(DragonChecksum.generateData("111100001010") == "1111000010100101011110000")
      }
    }
  }

  "checksum for 110010110100" should {
    "be 100" in {
      assert(DragonChecksum.generateChecksum("110010110100") == "100")
    }
  }

  "filling a disk of length 20 using an initial state of 10000" should {
    "generate a checksum of 01100" in {
      assert(DragonChecksum.fillAndGenerateChecksum(20, "10000") == "01100")
    }
  }
}
