import org.scalatest.FlatSpec

class BunnyTrianglesTests extends FlatSpec {

  "given example" should "return 0 valid triangles" in {
    val input =
      """
        |5 10 25
      """.stripMargin

    val lines = input.split("\n")
    assert(BunnyTriangles.countValidTrianglesHorizontally(lines) == 0)
  }

  "my example" should "return 2 valid triangles" in {
    val input =
      """
        |3  5  4
        |10 8 6
      """.stripMargin

    val lines = input.split("\n")
    assert(BunnyTriangles.countValidTrianglesHorizontally(lines) == 2)
  }
}
