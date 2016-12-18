object Main {
  def main(args: Array[String]): Unit = {

    val seed = 1362
    val maze = new Maze(seed)
    val startingPoint: Location = Location(1, 1)

    val answer = maze.bestPathLength(startingPoint, Location(31, 39))
    println(s"answer: $answer")

    val answer2 = (for {
      x <- 0 to 49
      y <- 0 to 49
      location = Location(x, y)
      if maze.locationToCubicle(location).value == CubicleType.OpenSpace
    } yield location).flatMap(maze.bestPathLength(startingPoint, _)) count (_ <= 50)
    println(s"answer2: $answer2")
  }
}
