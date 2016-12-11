import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day2-input.txt").getLines.toList

    val answer1 = BunnyBathroom.figureOutTheCode(NormalKeyPad)(lines)
    println(s"NormalKeyPad answer: $answer1")

    val answer2 = BunnyBathroom.figureOutTheCode(ExtendedKeyPad)(lines)
    println(s"ExtendedKeyPad answer: $answer2")
  }
}
