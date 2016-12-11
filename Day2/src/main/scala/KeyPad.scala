trait KeyPad {

  def move(c: Char, n: Int): Int =
    c match {
      case 'U' => KeyPadUps(n)
      case 'D' => KeyPadDowns(n)
      case 'L' => KeyPadLefts(n)
      case 'R' => KeyPadRights(n)
    }

  val KeyPadUps: Map[Int, Int]
  val KeyPadDowns: Map[Int, Int]
  val KeyPadLefts: Map[Int, Int]
  val KeyPadRights: Map[Int, Int]
}
