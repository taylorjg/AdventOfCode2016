object Main {
  def main(args: Array[String]): Unit = {

    val puzzleInput = "10001001100000001"

    val answer1 = DragonChecksum.fillAndGenerateChecksum(272, puzzleInput)
    println(s"answer1: $answer1")

    val answer2 = DragonChecksum.fillAndGenerateChecksum(35651584, puzzleInput)
    println(s"answer2: $answer2")
  }
}
