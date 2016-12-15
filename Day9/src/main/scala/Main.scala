import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day9-input.txt").mkString
    val decompressed = Decompression.decompress(input)
    val answer = decompressed.length
    println(s"Decompressed length: $answer")
  }
}
