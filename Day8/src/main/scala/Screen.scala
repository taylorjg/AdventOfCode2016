import Screen._

class Screen(cs: Int, rs: Int, val matrix: Vector[Boolean]) {

  def this(cs: Int, rs: Int) {
    this(cs, rs, Vector.fill(cs * rs)(false))
  }

  def this(cs: Int, rs: Int, s: String) {
    this(cs, rs, stringToMatrix(s))
  }

  def processInstruction(instruction: String): Screen =
    instruction match {
      case _ if instruction.startsWith("rect") => processRect(instruction)
      case _ if instruction.startsWith("rotate column") => processRotateColumn(instruction)
      case _ if instruction.startsWith("rotate row") => processRotateRow(instruction)
      case _ => throw new Exception(s"""Unexpected instruction, "$instruction".""")
    }

  private def processRect(instruction: String): Screen = {
    val m = RectRegex.findAllIn(instruction)
    val w = m.group(1).toInt
    val h = m.group(2).toInt
    val newMatrix = Vector.tabulate(rs, cs)((y, x) => x < w && y < h).flatten
    val finalMatrix = mergeMatricesLogicalOr(matrix, newMatrix, cs)
    new Screen(cs, rs, finalMatrix)
  }

  private def processRotateColumn(instruction: String): Screen = {
    val m = RotateColumnRegex.findAllIn(instruction)
    val y = m.group(1).toInt
    val by = m.group(2).toInt
    val oldCol = extractColumn(matrix, cs, y)
    val newCol = rotateRight(oldCol, by)
    val newMatrix = insertColumn(matrix, cs, y, newCol)
    new Screen(cs, rs, newMatrix)
  }

  private def processRotateRow(instruction: String): Screen = {
    val m = RotateRowRegex.findAllIn(instruction)
    val x = m.group(1).toInt
    val by = m.group(2).toInt
    val oldRow = extractRow(matrix, cs, x)
    val newRow = rotateRight(oldRow, by)
    val newMatrix = insertRow(matrix, cs, x, newRow)
    new Screen(cs, rs, newMatrix)
  }
}

object Screen {

  def matrixToString(m: Vector[Boolean]): String =
    (m map (if (_) "#" else ".")).mkString

  private def stringToMatrix(s: String): Vector[Boolean] = {
    val s2 = s.replaceAll("""\s""", "")
    Vector.tabulate(s2.length)(s2(_) == '#')
  }

  private def extractColumn(m: Vector[Boolean], cs: Int, col: Int): Vector[Boolean] =
    m.zipWithIndex collect {
      case (b, index) if index % cs == col => b
    }

  private def extractRow(m: Vector[Boolean], cs: Int, row: Int): Vector[Boolean] =
    m.zipWithIndex collect {
      case (b, index) if index / cs == row => b
    }

  private def insertColumn(m: Vector[Boolean], cs: Int, col: Int, v: Vector[Boolean]): Vector[Boolean] =
    m.zipWithIndex map {
      case (b, index) => {
        val x = index % cs
        val y = index / cs
        if (x == col) v(y) else b
      }
    }

  private def insertRow(m: Vector[Boolean], cs: Int, row: Int, v: Vector[Boolean]): Vector[Boolean] =
    m.zipWithIndex map {
      case (b, index) => {
        val x = index % cs
        val y = index / cs
        if (y == row) v(x) else b
      }
    }

  private def rotateRight(vector: Vector[Boolean], by: Int): Vector[Boolean] = {
    @annotation.tailrec
    def loop(remaining: Int, v: Vector[Boolean]): Vector[Boolean] =
      if (remaining == 0) v else loop(remaining - 1, v.last +: v.init)
    loop(by, vector)
  }

  private def mergeMatricesLogicalOr(m1: Vector[Boolean],
                                     m2: Vector[Boolean],
                                     cs: Int): Vector[Boolean] =
    mergeMatrices(m1, m2, cs, (_, _, b1, b2) => b1 | b2)

  private def mergeMatrices(m1: Vector[Boolean],
                            m2: Vector[Boolean],
                            cs: Int,
                            f: (Int, Int, Boolean, Boolean) => Boolean): Vector[Boolean] =
    m1.zipWithIndex.zip(m2) map {
      case ((b1, index), b2) => {
        val x = index % cs
        val y = index / cs
        f(x, y, b1, b2)
      }
    }

  private final val RectRegex = """rect (\d+)x(\d+)""".r
  private final val RotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r
  private final val RotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
}
