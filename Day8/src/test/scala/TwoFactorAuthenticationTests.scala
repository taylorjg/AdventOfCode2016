import org.scalatest.FlatSpec

class TwoFactorAuthenticationTests extends FlatSpec {

  "rect instruction" should "give the correct result" in {
    val instruction = "rect 3x2"
    val screen1 = new Screen(7, 3)
    val screen2 = screen1.processInstruction(instruction)
    val actual = Screen.matrixToString(screen2.matrix)
    val expected =
      """
        |###....
        |###....
        |.......
      """.stripMargin.replaceAll("""\s""", "")
    assert(actual == expected)
  }

  "rotate column instruction" should "give the correct result" in {
    val initialMatrix =
      """
        |###....
        |###....
        |.......
      """.stripMargin
    val instruction = "rotate column x=1 by 1"
    val screen1 = new Screen(7, 3, initialMatrix)
    val screen2 = screen1.processInstruction(instruction)
    val actual = Screen.matrixToString(screen2.matrix)
    val expected =
      """
        |#.#....
        |###....
        |.#.....
      """.stripMargin.replaceAll("""\s""", "")
    assert(actual == expected)
  }

  "rotate row instruction" should "give the correct result" in {
    val initialMatrix =
      """
        |#.#....
        |###....
        |.#.....
      """.stripMargin
    val instruction = "rotate row y=0 by 4"
    val screen1 = new Screen(7, 3, initialMatrix)
    val screen2 = screen1.processInstruction(instruction)
    val actual = Screen.matrixToString(screen2.matrix)
    val expected =
      """
        |....#.#
        |###....
        |.#.....
      """.stripMargin.replaceAll("""\s""", "")
    assert(actual == expected)
  }

  "wrapping rotate column instruction" should "give the correct result" in {
    val initialMatrix =
      """
        |....#.#
        |###....
        |.#.....
      """.stripMargin
    val instruction = "rotate column x=1 by 1"
    val screen1 = new Screen(7, 3, initialMatrix)
    val screen2 = screen1.processInstruction(instruction)
    val actual = Screen.matrixToString(screen2.matrix)
    val expected =
      """
        |.#..#.#
        |#.#....
        |.#.....
      """.stripMargin.replaceAll("""\s""", "")
    assert(actual == expected)
  }

  "list of instructions" should "give the correct result" in {
    val instructions = Seq(
      "rect 3x2",
      "rotate column x=1 by 1",
      "rotate row y=0 by 4",
      "rotate column x=1 by 1"
    )
    val screen = Screen.processInstructions(7, 3, instructions)
    val actual = Screen.matrixToString(screen.matrix)
    val expected =
      """
        |.#..#.#
        |#.#....
        |.#.....
      """.stripMargin.replaceAll("""\s""", "")
    assert(actual == expected)
  }
}
