object ErrorCorrection {

  private final val Ascending = Ordering[Int]
  private final val Descending = Ordering[Int].reverse

  def correct(lines: List[String], useMostCommon: Boolean): String = {
    val ordering = if (useMostCommon) Descending else Ascending
    def charOfInterest(col: Int): Char = {
      val sorted = (lines map (_ (col))).groupBy(identity).toList.sortBy(_._2.length)(ordering)
      sorted.head._1
    }
    @annotation.tailrec
    def loop(numCols: Int, acc: String): String = {
      if (acc.length == numCols) acc
      else {
        val col = acc.length
        val c = charOfInterest(col)
        loop(numCols, acc + c)
      }
    }
    loop(lines.head.length, "")
  }
}
