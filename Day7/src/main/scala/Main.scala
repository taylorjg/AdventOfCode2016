import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day7-input.txt").getLines.toList

    val answer1 = lines map InternetProtocolVersion7.supportsTLS count (_ == true)
    println(s"answer1: $answer1")

    val answer2 = lines map InternetProtocolVersion7.supportsSSL count (_ == true)
    println(s"answer2: $answer2")
  }
}
