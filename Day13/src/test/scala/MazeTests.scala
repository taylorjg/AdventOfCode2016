import org.scalatest.FlatSpec

class MazeTests extends FlatSpec {

  "the maze generated when the seed is 10" should "have a top left 10 x 7 corner like this" in {
    val expected =
      """
        |.#.####.##
        |..#..#...#
        |#....##...
        |###.#.###.
        |.##..#..#.
        |..##....#.
        |#...##.###
      """.stripMargin.split("\n") map (_.trim) filter (_.nonEmpty)
    val seed = 10
    val maze = new Maze(seed)
    val cubicles = for {
      y <- 0 to 6
      x <- 0 to 9
    } yield maze.locationToCubicle(Location(x, y))
    val actual = Maze.cubiclesToStrings(cubicles, 10).toArray
    assert(actual sameElements expected)
  }

  "(1,1) to (7,4)" should "take 11 steps" in {
    val seed = 10
    val maze = new Maze(seed)
    assert(maze.bestPathLength(Location(1, 1), Location(7, 4)) == Some(11))
  }
}
