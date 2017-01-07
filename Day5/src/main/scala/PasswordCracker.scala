import scala.util.{Success, Try}

object PasswordCracker {

  def crackPassword1(s: String): String = {
    @annotation.tailrec
    def loop(num: Int, acc: String): String = {
      if (acc.length == 8) acc
      else {
        val hash = calculateHash(s, num)
        if (hash.startsWith("00000")) {
          val newAcc = acc + hash(5).toString
          println(s"newAcc: $newAcc; num: $num")
          loop(num + 1, newAcc)
        }
        else loop(num + 1, acc)
      }
    }
    loop(0, "")
  }

  def crackPassword2(s: String): String = {
    def loop(num: Int, map: Map[Int, Char]): Map[Int, Char] = {
      if (map.size == 8) map
      else {
        val hash = calculateHash(s, num)
        if (hash.startsWith("00000")) {
          Try { Integer.parseInt(hash(5).toString) } match {
            case Success(pos) if pos <= 7 && !map.contains(pos) =>
              println(s"found char ${hash(6)} at pos $pos")
              loop(num + 1, map + (pos -> hash(6)))
            case _ =>
              loop(num + 1, map)
          }
        }
        else loop(num + 1, map)
      }
    }
    loop(0, Map.empty).toList.sortBy(kvp => kvp._1).map(_._2).mkString
  }

  private final val MessageDigest = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(s: String, num: Int): String = {
    val b = s"$s$num".getBytes("UTF-8")
    MessageDigest.update(b, 0, b.length)
    MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
