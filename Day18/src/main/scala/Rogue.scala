object Rogue {

  def getRows(firstRow: String, numRows: Int): Seq[String] =
    (1 until numRows).foldLeft(List(firstRow))((acc, _) => nextRow(acc.head) :: acc).reverse

  private def nextRow(previousRow: String): String = {
    def tileAt(pos: Int): Char = if (previousRow.isDefinedAt(pos)) previousRow(pos) else Safe
    def applyRules(pos: Int): Char = {
      val left = tileAt(pos - 1)
      val centre = tileAt(pos)
      val right = tileAt(pos + 1)
      def test(t1: Char, t2: Char, t3: Char): Boolean = left == t1 && centre == t2 && right == t3
      val rule1 = test(Trap, Trap, Safe)
      val rule2 = test(Safe, Trap, Trap)
      val rule3 = test(Trap, Safe, Safe)
      val rule4 = test(Safe, Safe, Trap)
      if (rule1 || rule2 || rule3 || rule4) Trap else Safe
    }
    (previousRow.indices map applyRules).mkString
  }

  private final val Safe = '.'
  private final val Trap = '^'
}
