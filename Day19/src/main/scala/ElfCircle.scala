object ElfCircle {

  def allPresentsGoTo1(numElves: Int): Int = {
    val c = Array.tabulate(numElves)(idx => (idx + 1, 1, (idx + 1) % numElves))
    @annotation.tailrec
    def loop(ei: Int): Int =
      c collectFirst { case (en, np, _) if np == numElves => en } match {
        case Some(en) => en
        case None =>
          val (en, np1, nei1) = c(ei)
          val (_, np2, nei2) = c(nei1)
          c(ei) = (en, np1 + np2, nei2)
          loop(nei2)
      }
    loop(0)
  }

  def allPresentsGoTo2(numElves: Int): Int = {
    @annotation.tailrec
    def loop(p: Int): Int = if (Math.pow(2, p) > numElves) p - 1 else loop(p + 1)
    val x = loop(0)
    val diff = numElves - Math.pow(2, x).toInt
    diff * 2 + 1
  }
}
