import Screen._

class Screen(width: Int, height: Int, val matrix: Vector[Boolean]) {

  def this(width: Int, height: Int) {
    this(width, height, Vector.fill(width * height)(false))
  }

  def this(width: Int, height: Int, s: String) {
    this(width, height, stringToMatrix(s))
  }

  def processInstruction(instruction: String): Screen =
    instruction match {
      case RectRegex(w, h) => processRect(w.toInt, h.toInt)
      case RotateColumnRegex(x, by) => processRotateColumn(x.toInt, by.toInt)
      case RotateRowRegex(y, by) => processRotateRow(y.toInt, by.toInt)
      case _ => throw new Exception(s"""Unexpected instruction, "$instruction".""")
    }

  private def processRect(rectWidth: Int, rectHeight: Int): Screen = {
    val rectMatrix = Vector.tabulate(width, height)(_ < rectWidth && _ < rectHeight).transpose.flatten
    val newMatrix = mergeMatricesUsingLogicalOr(matrix, rectMatrix)
    new Screen(width, height, newMatrix)
  }

  private def processRotateColumn(x: Int, by: Int): Screen = {
    val oldCol = extractColumn(matrix, width, x)
    val newCol = rotateRight(oldCol, by)
    val newMatrix = insertColumn(matrix, width, x, newCol)
    new Screen(width, height, newMatrix)
  }

  private def processRotateRow(y: Int, by: Int): Screen = {
    val oldRow = extractRow(matrix, width, y)
    val newRow = rotateRight(oldRow, by)
    val newMatrix = insertRow(matrix, width, y, newRow)
    new Screen(width, height, newMatrix)
  }
}

object Screen {

  def matrixToString(m: Vector[Boolean]): String =
    (m map (if (_) "#" else ".")).mkString

  def processInstructions(width: Int, height: Int, instructions: Seq[String]): Screen =
    instructions.foldLeft(new Screen(width, height))(_.processInstruction(_))

  private def stringToMatrix(s: String): Vector[Boolean] = {
    val s2 = s.replaceAll("""\s""", "")
    Vector.tabulate(s2.length)(s2(_) == '#')
  }

  private def extractColumn(m: Vector[Boolean], width: Int, col: Int): Vector[Boolean] =
    m.zipWithIndex collect {
      case (b, index) if index % width == col => b
    }

  private def extractRow(m: Vector[Boolean], width: Int, row: Int): Vector[Boolean] =
    m.zipWithIndex collect {
      case (b, index) if index / width == row => b
    }

  private def insertColumn(m: Vector[Boolean], width: Int, col: Int, v: Vector[Boolean]): Vector[Boolean] =
    m.zipWithIndex map {
      case (b, index) => {
        val x = index % width
        val y = index / width
        if (x == col) v(y) else b
      }
    }

  private def insertRow(m: Vector[Boolean], width: Int, row: Int, v: Vector[Boolean]): Vector[Boolean] =
    m.zipWithIndex map {
      case (b, index) => {
        val x = index % width
        val y = index / width
        if (y == row) v(x) else b
      }
    }

  private def rotateRight(vector: Vector[Boolean], by: Int): Vector[Boolean] = {
    @annotation.tailrec
    def loop(v: Vector[Boolean], n: Int): Vector[Boolean] =
      if (n == 0) v else loop(v.last +: v.init, n - 1)
    loop(vector, by)
  }

  private def mergeMatricesUsingLogicalOr(m1: Vector[Boolean], m2: Vector[Boolean]): Vector[Boolean] =
    m1.zip(m2) map { case (b1, b2) => b1 | b2 }

  private final val RectRegex = """rect (\d+)x(\d+)""".r
  private final val RotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r
  private final val RotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
}
