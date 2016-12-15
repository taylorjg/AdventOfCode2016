class Screen(cs: Int, rs: Int, val matrix: Vector[Boolean]) {

  def this(cs: Int, rs: Int) {
    this(cs, rs, Vector.fill(cs * rs)(false))
  }

  def this(cs: Int, rs: Int, s: String) {
    this(cs, rs, Screen.stringToMatrix(s))
  }

  def processInstruction(instruction: String): Screen =
    instruction match {
      case _ if instruction.startsWith("rect") => processRect(instruction)
      case _ if instruction.startsWith("rotate column") => processRotateColumn(instruction)
      case _ if instruction.startsWith("rotate row") => processRotateRow(instruction)
      case _ => throw new Exception(s"""Unexpected instruction, "$instruction".""")
    }

  private def processRect(instruction: String): Screen = {
    val m = Screen.RectRegex.findAllIn(instruction)
    val w = m.group(1).toInt
    val h = m.group(2).toInt
    val newMatrix = Vector.tabulate(rs, cs)((y, x) => x < w && y < h).flatten
    val finalMatrix = Screen.mergeMatrices(matrix, newMatrix)
    new Screen(cs, rs, finalMatrix)
  }

  private def processRotateColumn(instruction: String): Screen = {
    val m = Screen.RotateColumnRegex.findAllIn(instruction)
    val y = m.group(1).toInt
    val by = m.group(2).toInt
    // val oldCol = extractCol(y)
    // val newCol = rotate(oldCol, by)
    // val newMatrix = insertCol(newCol, y)
    // new Screen(cs, rs, newMatrix)
    this
  }

  private def processRotateRow(instruction: String): Screen = {
    val m = Screen.RotateRowRegex.findAllIn(instruction)
    val x = m.group(1).toInt
    val by = m.group(2).toInt
    this
  }
}

object Screen {

  def matrixToString(m: Vector[Boolean]): String =
    (m map (if (_) "#" else ".")).mkString

  private def stringToMatrix(s: String): Vector[Boolean] = {
    val s2 = s.replaceAll("""\s""", "")
    Vector.tabulate(s2.length)(s2(_) == '#')
  }

  private def mergeMatrices(m1: Vector[Boolean], m2: Vector[Boolean]): Vector[Boolean] =
    m1.zip(m2) map { case (a, b) => a | b }

  private final val RectRegex = """rect (\d+)x(\d+)""".r
  private final val RotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r
  private final val RotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
}
