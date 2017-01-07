import HashType._

object OneTimePad {

  def findIndex(salt: String, hashType: HashType.Value): Int = {

    type Cache = Map[Int, String]

    def findXxx(hash: String): Option[String] =
      hash match {
        case XxxRegex(x) => Some(x)
        case _ => None
      }

    def findXxxxx(x: Char, fromIndex: Int, toIndex: Int, initialCache: Cache): (Option[Int], Option[String], Cache) = {
      @annotation.tailrec
      def loop(index: Int, cache: Cache): (Option[Int], Option[String], Cache) = {
        if (index > toIndex) (None, None, cache)
        else {
          val (hash, updatedCache) = cache.get(index) match {
            case Some(h) =>
              (h, cache)
            case None =>
              val h = calculateHash(salt, index, hashType)
              (h, cache + (index -> h))
          }
          val dynamicXxxxxRegex = s".*?($x)\\1{4}.*".r
          hash match {
            case dynamicXxxxxRegex(_) => (Some(index), Some(hash), updatedCache)
            case _ => loop(index + 1, updatedCache)
          }
        }
      }
      loop(fromIndex, initialCache)
    }

    @annotation.tailrec
    def loop(index: Int, keys: List[String], cache: Cache): Int = {
      val (hash, updatedCache1) = cache.get(index) match {
        case Some(h) =>
          (h, cache - index)
        case None =>
          val h = calculateHash(salt, index, hashType)
          (h, cache)
      }
      findXxx(hash) match {
        case Some(xxx) =>
          findXxxxx(xxx.head, index + 1, index + 1000, updatedCache1) match {
            case (Some(index5), Some(hash5), updatedCache2) =>
              val newKeys = keys :+ hash
              val numKeys = newKeys.length
              println(s"Found key number $numKeys $hash/$index, $hash5/$index5")
              if (numKeys == 64) index
              else loop(index + 1, newKeys, updatedCache2)
            case (None, None, updatedCache2) => loop(index + 1, keys, updatedCache2)
            case _ => throw new Exception("BOOM")
          }
        case None =>
          loop(index + 1, keys, updatedCache1)
      }
    }

    loop(0, List(), Map())
  }

  private final val XxxRegex = """.*?([a-z0-9])\1{2}.*""".r
  private final val MessageDigest = java.security.MessageDigest.getInstance("MD5")

  def calculateHash(salt: String, index: Int, hashType: HashType.Value): String = {
    val initialHash = calculateHash(s"$salt$index")
    val additionalHashCount = if (hashType == Normal) 0 else 2016
    (1 to additionalHashCount).foldLeft(initialHash)((acc, _) => calculateHash(acc))
  }

  private def calculateHash(s: String): String = {
    val bytes = s.getBytes("UTF-8")
    MessageDigest.update(bytes, 0, bytes.length)
    MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
