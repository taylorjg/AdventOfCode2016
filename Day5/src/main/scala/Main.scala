object Main {
  def main(args: Array[String]): Unit = {

    val answer1 = PasswordCracker.crackPassword1("reyedfim")
    println(s"answer1: $answer1")

    val answer2 = PasswordCracker.crackPassword2("reyedfim")
    println(s"answer2: $answer2")
  }
}
