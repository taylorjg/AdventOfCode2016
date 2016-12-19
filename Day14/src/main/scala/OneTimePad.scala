object OneTimePad {

  def findIndex(salt: String, hashType: HashType.Value): Int = {

    type Cache = Map[Int, String]

    def findXxx(hash: String): Option[String] = {
      if (XxxRegex.pattern.matcher(hash).matches()) {
        val m = XxxRegex.findAllIn(hash)
        Some(m.group(1))
      }
      else None
    }

    def findXxxxx(x: Char, fromIndex: Int, toIndex: Int, initialCache: Cache): (Option[Int], Cache) = {
      @annotation.tailrec
      def loop(index: Int, cache: Cache): (Option[Int], Cache) = {
        if (index == toIndex) (None, cache)
        else {
          val (hash, updatedCache) = cache.get(index) match {
            case Some(h) =>
              (h, cache)
            case None =>
              val h = calculateHash(salt, index, hashType)
              (h, cache + (index -> h))
          }
          val r = s".*($x)\\1{4}.*".r
          if (r.pattern.matcher(hash).matches()) (Some(index), updatedCache)
          else loop(index + 1, updatedCache)
        }
      }
      loop(fromIndex, initialCache)
    }

    @annotation.tailrec
    def loop(index: Int, nth: Int, cache: Cache): Int = {
      if (nth == 65) index - 1
      else {
        val (hash, updatedCache1) = cache.get(index) match {
          case Some(h) =>
            (h, cache - index)
          case None =>
            val h = calculateHash(salt, index, hashType)
            (h, cache + (index -> h))
        }
        findXxx(hash) match {
          case Some(xxx) =>
            findXxxxx(xxx.head, index + 1, index + 1000, updatedCache1) match {
              case (Some(_), updatedCache2) => {
                println(s"Found key $nth")
                loop(index + 1, nth + 1, updatedCache2)
              }
              case (None, updatedCache2) => loop(index + 1, nth, updatedCache2)
            }
          case None =>
            loop(index + 1, nth, updatedCache1)
        }
      }
    }

    loop(0, 0, Map.empty)
  }

  private final val XxxRegex = """.*([a-z0-9])\1{2}.*""".r
  private final val MessageDigest = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(salt: String, index: Int, hashType: HashType.Value): String = {
    val bytes = s"$salt$index".getBytes("UTF-8")
    MessageDigest.update(bytes, 0, bytes.length)
    MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
