import CubicleType._

class Maze(seed: Int) {

  def locationToCubicle(location: Location): Cubicle = {
    val x = location.x
    val y = location.y
    val v1 = (x * x) + (3 * x) + (2 * x * y) + y + (y * y)
    val v2 = v1 + seed
    val v3 = v2.toBinaryString
    val numOnes = v3 count (_ == '1')
    Cubicle(if (isEven(numOnes)) OpenSpace else Wall)
  }

  // https://en.wikipedia.org/wiki/A*_search_algorithm
  // http://web.mit.edu/eranki/www/tutorials/search/
  def bestPathPength(start: Location, goal: Location): Int = {

    case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
      val f = g + h
    }

    def distance(l1: Location, l2: Location): Double = {
      val x = l1.x - l2.x
      val y = l1.y - l2.y
      Math.hypot(x, y)
    }

    def getOpenSpaceNeighbours(location: Location): Seq[Location] = {
      val x1 = location.x
      val y1 = location.y
      for {
        (dx, dy) <- Seq((0, 1), (0, -1), (1, 0), (-1, 0))
        x2 = x1 + dx
        y2 = y1 + dy
        if x2 >= 0 && y2 >= 0
        neighbour = Location(x2, y2)
        if locationToCubicle(neighbour).value == OpenSpace
      } yield neighbour
    }

    def makeNeighbourNode(parent: Node)(neighbourLocation: Location): Node =
      Node(
        neighbourLocation,
        Some(parent),
        parent.g + distance(parent.location, neighbourLocation),
        distance(neighbourLocation, goal))

    @annotation.tailrec
    def aStar(openSet: Set[Node], closedSet: Set[Node]): Node = {
      if (openSet.isEmpty) throw new Exception("Oh dear - we have failed to find a path!")
      val current = openSet.minBy(_.f)
      val newOpenSet = openSet - current
      val newClosedSet = closedSet + current
      if (current.location == goal) current
      else {
        val neighbourLocations = getOpenSpaceNeighbours(current.location)
        val neighbourNodes = neighbourLocations map makeNeighbourNode(current)
        def betterNode(nn: Node)(n: Node): Boolean = n.location == nn.location && n.f < nn.f
        val filteredNeighbourNodes = neighbourNodes filter (nn =>
          !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
        aStar(newOpenSet ++ filteredNeighbourNodes, newClosedSet)
      }
    }

    def pathLength(node: Node): Int = {
      @annotation.tailrec
      def loop(n: Node, acc: Int): Int =
        n.parent match {
          case Some(child) => loop(child, acc + 1)
          case None => acc
        }
      loop(node, 0)
    }

    val startNode = Node(start, None, 0, 0)
    val winningNode = aStar(openSet = Set(startNode), closedSet = Set())
    pathLength(winningNode)
  }

  private def isEven(v: Int): Boolean = v % 2 == 0
}

object Maze {
  def cubiclesToStrings(cubicles: Seq[Cubicle], width: Int): Seq[String] =
    (cubicles collect {
      case c if c.value == Wall => "#"
      case c if c.value == OpenSpace => "."
    }).mkString.grouped(width).toList
}
