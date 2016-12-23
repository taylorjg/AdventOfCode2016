object Main {
  def main(args: Array[String]): Unit = {

    val puzzleInput = 3005290

    val answer1 = ElfCircle.allPresentsGoToLeft2(puzzleInput)
    println(s"answer1: $answer1")

    val answer2 = ElfCircle.allPresentsGoToAcross1(puzzleInput)
    println(s"answer2: $answer2")
  }
}
