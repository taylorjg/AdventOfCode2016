import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day4-input.txt").getLines.toList

    val xs = lines map EncryptedRoomNames.validateRoom
    val sum = xs.flatten.map(_._2).sum
    println(s"sum of the sector IDs of the real rooms: $sum")

    val decryptedRoomNames = xs.flatten.map(x => (EncryptedRoomNames.decryptRoomName(x._1, x._2), x._2))
    val maybeDesiredRoomName = decryptedRoomNames find (x => x._1.toLowerCase.contains("north") && x._1.toLowerCase.contains("pole"))
    maybeDesiredRoomName match {
      case Some((roomName, sectorId)) => println(s"Found desired room! room name: $roomName; sectorId: $sectorId")
      case _ => println("Failed to find desired room!")
    }
  }
}
