object OneTimePad {

  def findIndex(salt: String): Int = {

    def findXxx(hash: String): Option[String] = {
      val r = """.*([a-z0-9])\1{2}.*""".r
      if (r.pattern.matcher(hash).matches()) {
        val m = r.findAllIn(hash)
        Some(m.group(1))
      }
      else None
    }

    def findXxxxx(x: Char, fromIndex: Int, toIndex: Int): Option[Int] = {
      @annotation.tailrec
      def loop(index: Int): Option[Int] = {
        if (index == toIndex) None
        else {
          val hash = calculateHash(salt, index)
          val r = s".*($x)\\1{4}.*".r
          if (r.pattern.matcher(hash).matches()) Some(index)
          else loop(index + 1)
        }
      }
      loop(fromIndex)
    }

    @annotation.tailrec
    def loop(index: Int, nth: Int): Int = {
      if (nth == 65) index -1
      else {
        val hash = calculateHash(salt, index)
        findXxx(hash) match {
          case Some(xxx) =>
            findXxxxx(xxx.head, index + 1, index + 1000) match {
              case Some(_) => {
                println(s"Found key $nth")
                loop(index + 1, nth + 1)
              }
              case None => loop(index + 1, nth)
            }
          case None =>
            loop(index + 1, nth)
        }
      }
    }

    loop(0, 0)
  }

  private final val messageDigest = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(salt: String, index: Int): String = {
    val bytes = s"$salt$index".getBytes("UTF-8")
    messageDigest.update(bytes, 0, bytes.length)
    messageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
