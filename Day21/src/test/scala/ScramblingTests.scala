import Scrambling._
import org.scalatest.FlatSpec

class ScramblingTests extends FlatSpec {

  "SwapPosition" should "work correctly" in {
    val operation = SwapPosition(4, 0)
    assert(Scrambling.swapPosition("abcde", operation.x, operation.y) == "ebcda")
  }

  "SwapLetter" should "work correctly" in {
    val operation = SwapLetter('d', 'b')
    assert(Scrambling.swapLetter("abcde", operation.x, operation.y) == "adcbe")
  }

  "RotateLeft" should "work correctly" in {
    val operation = RotateLeft(2)
    assert(Scrambling.rotateLeft("abcde", operation.x) == "cdeab")
  }

  "RotateRight" should "work correctly" in {
    val operation = RotateRight(2)
    assert(Scrambling.rotateRight("abcde", operation.x) == "deabc")
  }

  "RotateLetter 1" should "work correctly" in {
    val operation = RotateLetter('b')
    assert(Scrambling.rotateLetter("abcde", operation.x) == "deabc")
  }

  "RotateLetter 2" should "work correctly" in {
    val operation = RotateLetter('e')
    assert(Scrambling.rotateLetter("abcde", operation.x) == "eabcd")
  }

  "Reverse" should "work correctly" in {
    val operation = Reverse(1, 3)
    assert(Scrambling.reverse("abcde", operation.x, operation.y) == "adcbe")
  }

  "Move" should "work correctly" in {
    val operation = Move(1, 3)
    assert(Scrambling.move("abcde", operation.x, operation.y) == "acdbe")
  }

  "given example" should "result in decab" in {
    val input =
      """
        |swap position 4 with position 0
        |swap letter d with letter b
        |reverse positions 0 through 4
        |rotate left 1 step
        |move position 1 to position 4
        |move position 3 to position 0
        |rotate based on position of letter b
        |rotate based on position of letter d
      """.stripMargin
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val operations = Scrambling.parseLines(lines)
    operations foreach println
    assert(Scrambling.scramble("abcde", operations) == "decab")
  }
}
