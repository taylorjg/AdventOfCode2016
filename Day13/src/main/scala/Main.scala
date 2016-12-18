object Main {
  def main(args: Array[String]): Unit = {
    val seed = 1362
    val maze = new Maze(seed)
    val answer = maze.bestPathLength(Location(1, 1), Location(31, 39))
    println(s"answer: $answer")
  }
}
