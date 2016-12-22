import org.scalatest.FlatSpec

class ElfCircleTests extends FlatSpec {

  "5 elves" should "result in elf 3 having all the presents" in {
    assert(ElfCircle.allPresentsGoTo2(5) == 3)
  }

  "the formula version" should "give the same answer as the longhand version" in {
    val n = 200
    val xs = 1 to n map ElfCircle.allPresentsGoTo1
    val ys = 1 to n map ElfCircle.allPresentsGoTo2
    assert(xs == ys)
  }
}
