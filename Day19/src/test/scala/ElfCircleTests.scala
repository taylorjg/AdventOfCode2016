import org.scalatest.FlatSpec

class ElfCircleTests extends FlatSpec {

  "5 elves (left)" should "result in elf 3 having all the presents" in {
    assert(ElfCircle.allPresentsGoToLeft1(5) == 3)
  }

  "the formula version (left)" should "give the same answer as the longhand version" in {
    val n = 200
    val xs = 1 to n map ElfCircle.allPresentsGoToLeft1
    val ys = 1 to n map ElfCircle.allPresentsGoToLeft2
    assert(xs == ys)
  }

  "5 elves (across)" should "result in elf 2 having all the presents" in {
    assert(ElfCircle.allPresentsGoToAcross1(5) == 2)
  }
}
