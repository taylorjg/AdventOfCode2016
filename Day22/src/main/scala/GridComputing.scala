object GridComputing {

  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, use: Int)

  def parseLines(lines: Seq[String]): Seq[Node] = {
    def parseLine(line: String): Node =
      line match {
        case LineRegex(x, y, size, used, avail, use) =>
          Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt, use.toInt)
      }

    lines.drop(2) map parseLine
  }

  private final val LineRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  def viableNodes(nodes: Seq[Node]): Seq[(Node, Node)] = {
    def findNode(l: (Int, Int)): Node = {
      val (x, y) = l
      nodes.find(n => n.x == x && n.y == y).get
    }
    def isViable(ls: ((Int, Int), (Int, Int))): Option[(Node, Node)] = {
      val na = findNode(ls._1)
      val nb = findNode(ls._2)
      Some(na, nb) filter (_ => na.used > 0 && na.used < nb.avail)
    }
    val maxX = nodes.maxBy(n => n.x).x
    val maxY = nodes.maxBy(n => n.y).y
    val allCoords = for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield (x, y)
    val allPairs = for {
      la <- allCoords
      lb <- allCoords
      if la != lb
    } yield (la, lb)
    allPairs flatMap isViable
  }

  def fewestNumberOfSteps(nodes: Seq[Node]): Int = {
    val maxX = nodes.maxBy(n => n.x).x
    val maxY = nodes.maxBy(n => n.y).y
    val allCoords = for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield (x, y)
    -1
  }
}
