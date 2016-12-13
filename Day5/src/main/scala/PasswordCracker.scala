object PasswordCracker {

  def main(args: Array[String]): Unit = {
    val answer = crackPassword("reyedfim")
    println(s"answer: $answer")
  }

  def crackPassword(s: String): String = {
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

  private final val m = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(s: String, num: Int): String = {
    val b = s"$s$num".getBytes("UTF-8")
    m.update(b, 0, b.length)
    m.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
