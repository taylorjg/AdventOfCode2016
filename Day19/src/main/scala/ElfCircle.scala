import scala.collection.mutable.ArrayBuffer

object ElfCircle {

  def allPresentsGoToLeft1(numElves: Int): Int = {
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

  def allPresentsGoToLeft2(numElves: Int): Int = {
    @annotation.tailrec
    def loop(p: Int): Int = if (Math.pow(2, p) > numElves) p - 1 else loop(p + 1)

    val x = loop(0)
    val diff = numElves - Math.pow(2, x).toInt
    diff * 2 + 1
  }

  def allPresentsGoToAcross1(numElves: Int): Int = {
    val c = ArrayBuffer.tabulate(numElves)(idx => (idx + 1, 1))

    @annotation.tailrec
    def loop(ei: Int): Int = {
      // val xs = c.zipWithIndex
      // println(s"xs: $xs; ei: $ei")
      val len = c.length
      if (len % 10000 == 0 || len < 100) {
        println(s"len: $len")
      }
      if (len == 1) c.head._1
      else {
        val (en, np1) = c(ei)
        val aei = (ei + len / 2) % len
        val (_, np2) = c(aei)
        c(ei) = (en, np1 + np2)
        val nextIndex = (ei + 1) % len
        val nextIndexAdjusted = if (nextIndex == c.indices.last) ei else nextIndex
        c.remove(aei)
        loop(nextIndexAdjusted)
      }
    }

    loop(0)
  }
}

// Wrong: 32283
