import org.scalatest.FlatSpec

class AirDuctSpelunkingTests extends FlatSpec {
  "given example" should "find the shortest route" in {
    val input =
      """
        |###########
        |#0.1.....2#
        |#.#######.#
        |#4.......3#
        |###########
      """.stripMargin
    val lines = input.split("\n").map(_.trim)
    val airDuctSpelunking = new AirDuctSpelunking(lines)
    assert(airDuctSpelunking.shortestRoute() == 14)
  }
}
