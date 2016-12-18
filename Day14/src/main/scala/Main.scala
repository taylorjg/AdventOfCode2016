import HashType._

object Main {
  def main(args: Array[String]): Unit = {

    val answer1 = OneTimePad.findIndex("zpqevtbw", Normal)
    println(s"answer1: $answer1")

    val answer2 = OneTimePad.findIndex("zpqevtbw", Stretched)
    println(s"answer2: $answer2")
  }
}
