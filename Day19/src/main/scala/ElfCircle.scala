import scala.collection.mutable.ArrayBuffer

object ElfCircle {
  def allPresentsGoTo(numElves: Int): Int = {
    val arr = ArrayBuffer.tabulate(numElves)(n => (n + 1, 1))
    @annotation.tailrec
    def loop(elfIndex: Int): Int =
      arr collectFirst { case (en, np) if np == numElves => en } match {
        case Some(en) => en
        case None =>
          val nextElfIndex = (elfIndex + 1) % arr.length
          val (en, np1) = arr(elfIndex)
          val (_, np2) = arr(nextElfIndex)
          arr(elfIndex) = (en, np1 + np2)
          arr.remove(nextElfIndex)
          loop(nextElfIndex)
      }
    loop(0)
  }
}
