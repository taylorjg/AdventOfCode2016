object EncryptedRoomNames {

  private final val Regex = """(.*)-(\d+)\[(\w+)\]""".r

  def validateRoom(roomName: String): Option[(String, Int)] = {
    val ms = Regex.findAllIn(roomName)
    val encryptedName = ms.group(1)
    val sectorId = ms.group(2)
    val checksum = ms.group(3)
    val grouped = (encryptedName filter (_ != '-')) groupBy (c => c) map { case (c, cs) => (c, cs.length) }
    val groupedAndSorted = grouped.toList.sortWith((a, b) =>
      (a, b) match {
        case ((c1, l1), (c2, l2)) => if (l1 != l2) l1 > l2 else c1 < c2
      }).take(5)
    val expectedChecksum = (groupedAndSorted map (_._1)).mkString
    Some((encryptedName, sectorId.toInt)) filter (_ => checksum == expectedChecksum)
  }

  private final val z = 'z'.toInt

  def decryptRoomName(encryptedName: String, sectorId: Int): String = {
    def rotateLetter(c: Char): Char = {
      val v1 = c.toInt + (sectorId % 26)
      val v2 = v1 - (if (v1 > z) 26 else 0)
      v2.toChar
    }
    def decodeChar(c: Char): Char =
      c match {
        case '-' => ' '
        case _ => rotateLetter(c)
      }
    (encryptedName map decodeChar).mkString
  }
}
