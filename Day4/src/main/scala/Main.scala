import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4-input.txt").getLines.toList
    val sectorIds = lines map EncryptedRoomNames.sectorIdOfRoom
    val sum = sectorIds.flatten.sum
    println(s"sum: $sum")
  }
}
