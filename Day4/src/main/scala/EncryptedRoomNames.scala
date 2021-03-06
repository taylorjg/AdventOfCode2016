object EncryptedRoomNames {

  private final val Regex = """(.*)-(\d+)\[(\w+)\]""".r

  def validateRoom(roomName: String): Option[(String, Int)] =
    roomName match {
      case Regex(encryptedName, sectorId, checksum) =>
        val grouped = (encryptedName filter (_ != '-')) groupBy (c => c) map { case (c, cs) => (c, cs.length) }
        val groupedAndSorted = grouped.toList.sortWith((a, b) =>
          (a, b) match {
            case ((c1, l1), (c2, l2)) => if (l1 != l2) l1 > l2 else c1 < c2
          }).take(5)
        val expectedChecksum = (groupedAndSorted map (_._1)).mkString
        Some((encryptedName, sectorId.toInt)) filter (_ => checksum == expectedChecksum)
      case _ =>
        None
    }

  def decryptRoomName(encryptedName: String, sectorId: Int): String = {
    def rotateChar(c: Char): Char = {
      val v1 = c + (sectorId % 26)
      val v2 = v1 - (if (v1 > 'z') 26 else 0)
      v2.toChar
    }
    def decodeChar(c: Char): Char =
      c match {
        case '-' => ' '
        case _ => rotateChar(c)
      }
    (encryptedName map decodeChar).mkString
  }
}
