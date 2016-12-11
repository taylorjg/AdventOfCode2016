object BunnyBathroom {

  def figureOutTheCode(keyPad: KeyPad)(lines: Seq[String]): String = {

    def figureOutLine(line: String, startingPoint: Int): Int = {
      def loop(cs: Seq[Char], n: Int): Int =
        cs match {
          case c +: rest => loop(rest, keyPad.move(c, n))
          case _ => n
        }

      loop(line, startingPoint)
    }

    def loop(ls: Seq[String], startingPoint: Int, ns: Seq[Int]): Seq[Int] =
      ls match {
        case l +: rest =>
          val next = figureOutLine(l, startingPoint)
          loop(rest, next, next +: ns)
        case _ => ns
      }

    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)

    loop(cleanedLines, 5, Seq.empty)
      .reverse
      .map(_.toHexString)
      .mkString
      .toUpperCase
  }
}
