object ElfCircle {
  def allPresentsGoTo(numElves: Int): Int = {
    def nextElf(elf: Int): Int = (elf % numElves) + 1
    val m = collection.mutable.Map((1 to numElves) map (n => (n, (1, nextElf(n)))): _*)
    @annotation.tailrec
    def loop(elf: Int): Int =
      m collectFirst { case (k, v) if v._1 == numElves => k } match {
        case Some(winningElf) => winningElf
        case None =>
          val (a, b) = m(elf)
          val (c, d) = m(b)
          m(elf) = (a + c, d)
          m -= b
          println(s"elf: $elf; m(elf): ${m(elf)}")
          loop(d)
      }
    loop(1)
  }
}
