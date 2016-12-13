object EncryptedRoomNames {

  private final val Regex = """(.*)-(\d+)\[(\w+)\]""".r

  def sectorIdOfRoom(name: String): Option[Int] = {
    val ms = Regex.findAllIn(name)
    val encryptedName = ms.group(1)
    val sectorId = ms.group(2)
    val checksum = ms.group(3)
    val grouped = (encryptedName filter (_ != '-')) groupBy (c => c) map { case(c, cs) => (c, cs.length) }
    val groupedAndSorted = grouped.toList.sortWith((a, b) =>
      (a, b) match {
        case ((c1, l1), (c2, l2)) => if (l1 != l2) l1 > l2 else c1 < c2
      }).take(5)
    val expectedChecksum = (groupedAndSorted map (_._1)).mkString
    Some(sectorId.toInt) filter (_ => checksum == expectedChecksum)
  }
}
