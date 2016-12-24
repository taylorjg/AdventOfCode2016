object Scrambling {

  sealed trait Operation
  case class SwapPosition(x: Int, y: Int) extends Operation
  case class SwapLetter(x: Char, y: Char) extends Operation
  case class RotateLeft(x: Int) extends Operation
  case class RotateRight(x: Int) extends Operation
  case class RotateLetter(x: Char) extends Operation
  case class RotateLetterReverse(x: Char) extends Operation
  case class Reverse(x: Int, y: Int) extends Operation
  case class Move(x: Int, y: Int) extends Operation

  def parseLines(lines: Seq[String]): Seq[Operation] = {
    def parseLine(line: String): Operation =
      line match {
        case SwapPositionRegex(x, y) => SwapPosition(x.toInt, y.toInt)
        case SwapLetterRegex(x, y) => SwapLetter(x.head, y.head)
        case RotateLeftRegex(x) => RotateLeft(x.toInt)
        case RotateRightRegex(x) => RotateRight(x.toInt)
        case RotateLetterRegex(x) => RotateLetter(x.head)
        case ReverseRegex(x, y) => Reverse(x.toInt, y.toInt)
        case MoveRegex(x, y) => Move(x.toInt, y.toInt)
      }
    lines map parseLine
  }

  def reverseOperations(operations: Seq[Operation]): Seq[Operation] = {
    operations.map(reverseOperation).reverse
  }

  def reverseOperation(operation: Operation): Operation =
    operation match {
      case SwapPosition(x, y) => SwapPosition(y, x)
      case SwapLetter(x, y) => SwapLetter(y, x)
      case RotateLeft(x) => RotateRight(x)
      case RotateRight(x) => RotateLeft(x)
      case RotateLetter(x) => RotateLetterReverse(x)
      case RotateLetterReverse(_) => throw new Exception("This operation should not appear in the input")
      case Reverse(x, y) => Reverse(x, y)
      case Move(x, y) => Move(y, x)
    }

  private final val SwapPositionRegex = """swap position (\d+) with position (\d+)""".r
  private final val SwapLetterRegex = """swap letter ([a-z]) with letter ([a-z])""".r
  private final val RotateLeftRegex = """rotate left (\d+) steps?""".r
  private final val RotateRightRegex = """rotate right (\d+) steps?""".r
  private final val RotateLetterRegex = """rotate based on position of letter ([a-z])""".r
  private final val ReverseRegex = """reverse positions (\d+) through (\d+)""".r
  private final val MoveRegex = """move position (\d+) to position (\d+)""".r

  def scramble(s: String, operations: Seq[Operation]): String =
    operations.foldLeft(s)(applyOperation)

  def applyOperation(s: String, operation: Operation): String =
    operation match {
      case SwapPosition(x, y) => swapPosition(s, x, y)
      case SwapLetter(x, y) => swapLetter(s, y, x)
      case RotateLeft(x) => rotateLeft(s, x)
      case RotateRight(x) => rotateRight(s, x)
      case RotateLetter(x) => rotateLetter(s, x)
      case RotateLetterReverse(x) => rotateLetterReverse(s, x)
      case Reverse(x, y) => reverse(s, x, y)
      case Move(x, y) => move(s, x, y)
    }

  def swapPosition(s: String, x: Int, y: Int): String = {
    val c = s(x)
    s.updated(x, s(y)).updated(y, c)
  }

  def swapLetter(s: String, x: Char, y: Char): String = {
    val posX = s indexOf x
    val posY = s indexOf y
    swapPosition(s, posX, posY)
  }

  def rotateLeft(s: String, x: Int): String = {
    def rotateLeftOne(s: String): String = s.tail + s.head
    (1 to x).foldLeft(s)((acc, _) => rotateLeftOne(acc))
  }

  def rotateRight(s: String, x: Int): String = {
    def rotateRightOne(s: String): String = s.last + s.init
    (1 to x).foldLeft(s)((acc, _) => rotateRightOne(acc))
  }

  def rotateLetter(s: String, x: Char): String = {
    val posX = s indexOf x
    val numRotations = posX + 1 + (if (posX >= 4) 1 else 0)
    rotateRight(s, numRotations)
  }

  def rotateLetterReverse(s: String, x: Char): String = {
    @annotation.tailrec
    def loop(n: Int): String = {
      if (n == s.length) throw new Exception("Failed to reverse RotateLetter!")
      val unscrambled = rotateLeft(s, n)
      val rescrambled = rotateLetter(unscrambled, x)
      if (rescrambled == s) unscrambled else loop(n + 1)
    }
    loop(0)
  }

  def reverse(s: String, x: Int, y: Int): String = {
    val part1 = s.substring(0, x)
    val part2 = s.substring(x, y + 1).reverse
    val part3 = s.substring(y + 1)
    part1 + part2 + part3
  }

  def move(s: String, x: Int, y: Int): String = {
    val ch = s(x)
    val beforeRemovalPos = s.substring(0, x)
    val afterRemovalPos = s.substring(x + 1)
    val afterRemoval = beforeRemovalPos + afterRemovalPos
    val beforeInsertionPos = afterRemoval.substring(0, y)
    val afterInsertionPos = afterRemoval.substring(y)
    beforeInsertionPos + ch + afterInsertionPos
  }
}
