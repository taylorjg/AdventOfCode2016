import Scrambling._
import org.scalatest.FlatSpec

class ScramblingTests extends FlatSpec {

  "SwapPosition" should "work correctly" in {
    val operation = SwapPosition(4, 0)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "ebcda")
  }

  "SwapLetter" should "work correctly" in {
    val operation = SwapLetter('d', 'b')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "adcbe")
  }

  "RotateLeft" should "work correctly" in {
    val operation = RotateLeft(2)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "cdeab")
  }

  "RotateRight" should "work correctly" in {
    val operation = RotateRight(2)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "deabc")
  }

  "RotateLetter 1" should "work correctly" in {
    val operation = RotateLetter('b')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "deabc")
  }

  "RotateLetter 2" should "work correctly" in {
    val operation = RotateLetter('e')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "eabcd")
  }

  "Reverse" should "work correctly" in {
    val operation = Reverse(1, 3)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "adcbe")
  }

  "Move" should "work correctly" in {
    val operation = Move(1, 3)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "acdbe")
  }

  "abcde" should "scramble to decab" in {
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

  "reverse of SwapPosition" should "work correctly" in {
    val operation = SwapPosition(4, 0)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "ebcda")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of SwapLetter" should "work correctly" in {
    val operation = SwapLetter('d', 'b')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "adcbe")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of RotateLeft" should "work correctly" in {
    val operation = RotateLeft(2)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "cdeab")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of RotateRight" should "work correctly" in {
    val operation = RotateRight(2)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "deabc")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of RotateLetter 1" should "work correctly" in {
    val operation = RotateLetter('b')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "deabc")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of RotateLetter 2" should "work correctly" in {
    val operation = RotateLetter('e')
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "eabcd")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of Reverse" should "work correctly" in {
    val operation = Reverse(1, 3)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "adcbe")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "reverse of Move" should "work correctly" in {
    val operation = Move(1, 3)
    val scrambled = Scrambling.applyOperation("abcde", operation)
    assert(scrambled == "acdbe")
    val reverseOperation = Scrambling.reverseOperation(operation)
    val unscrambled = Scrambling.applyOperation(scrambled, reverseOperation)
    assert(unscrambled == "abcde")
  }

  "decab" should "unscramble to abcde" in {
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
    val reversedOperations = Scrambling.reverseOperations(operations)
    operations foreach println
    assert(Scrambling.scramble("decab", reversedOperations) == "abcde")
  }
}
