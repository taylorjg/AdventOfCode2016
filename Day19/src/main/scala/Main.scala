object Main {
  def main(args: Array[String]): Unit = {

    val answer1 = ElfCircle.allPresentsGoToLeft2(3005290)
    println(s"answer1: $answer1")

    val answer2 = ElfCircle.allPresentsGoToAcross1(3005290)
    println(s"answer2: $answer2")
  }
}
