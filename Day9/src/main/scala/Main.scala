import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val input = Source.fromResource("day9-input.txt").mkString

    val decompressed = Decompression.decompress(input)
    val answer1 = decompressed.length
    println(s"Decompressed length: $answer1")

    val answer2 = Decompression.decompressedLength(input)
    println(s"Decompressed length: $answer2")

    val answer3 = Decompression.decompressedLengthV2(input)
    println(s"Decompressed length (V2): $answer3")
  }
}
