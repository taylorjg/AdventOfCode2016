import org.scalatest.FlatSpec

class ElfCircleTests extends FlatSpec {
  "5 elves" should "result in elf 3 having all the presents" in {
    assert(ElfCircle.allPresentsGoTo(5) == 3)
  }
}
