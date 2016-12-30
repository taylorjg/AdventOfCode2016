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
    def isViable(ls: ((Int, Int), (Int, Int))): Option[(Node, Node)] = {
      val na = findNode(nodes, ls._1)
      val nb = findNode(nodes, ls._2)
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

  def visualise(nodes: Seq[Node]): Unit =
    visualise(nodes, (node, isTopLeft, isTopRight) => {
      val left = if (isTopLeft) "(" else " "
      val right = if (isTopLeft) ")" else " "
      val centre = (node.used, node.use, isTopRight) match {
        case (_, _, true) => "G"
        case (0, _, _) => "_"
        case (_, use, _) if use >= 90 => "#"
        case _ => "."
      }
      left + centre + right
    })

  private def visualise(nodes: Seq[Node], f: (Node, Boolean, Boolean) => String): Unit = {
    val maxX = nodes.maxBy(n => n.x).x
    val maxY = nodes.maxBy(n => n.y).y
    val lines = for {
      y <- 0 to maxY
    } yield {
      val bits = for {
        x <- 0 to maxX
        node = findNode(nodes, x, y)
        isTopLeft = x == 0 && y == 0
        isTopRight = x == maxX && y == 0
        fn = f(node, isTopLeft, isTopRight)
      } yield fn
      bits.mkString
    }
    lines foreach println
  }

  private def findNode(nodes: Seq[Node], l: (Int, Int)): Node =
    findNode(nodes, l._1, l._2)

  private def findNode(nodes: Seq[Node], x: Int, y: Int): Node =
    nodes.find(n => n.x == x && n.y == y).get
}
