import CubicleType._

class Maze(seed: Int) {

  def cubicleType(location: Location): CubicleType.Value = {
    val x = location.x
    val y = location.y
    val v = (x*x + 3*x + 2*x*y + y + y*y) + seed
    val binaryString = v.toBinaryString
    val numOnes = binaryString count (_ == '1')
    if (isEven(numOnes)) OpenSpace else Wall
  }

  // https://en.wikipedia.org/wiki/A*_search_algorithm
  // http://web.mit.edu/eranki/www/tutorials/search/
  def bestPathLength(start: Location, goal: Location): Option[Int] = {

    case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
      val f = g + h
    }

    def distance(l1: Location, l2: Location): Double = {
      val dx = l1.x - l2.x
      val dy = l1.y - l2.y
      Math.hypot(dx, dy)
    }

    def getOpenSpaceNeighbours(location: Location): Seq[Location] = {
      val x1 = location.x
      val y1 = location.y
      for {
        (dx, dy) <- Seq((0, 1), (0, -1), (-1, 0), (1, 0))
        (x2, y2) = (x1 + dx, y1 + dy)
        if x2 >= 0 && y2 >= 0
        neighbour = Location(x2, y2)
        if cubicleType(neighbour) == OpenSpace
      } yield neighbour
    }

    def makeNeighbourNode(parent: Node)(neighbourLocation: Location): Node =
      Node(
        neighbourLocation,
        Some(parent),
        parent.g + distance(parent.location, neighbourLocation),
        distance(neighbourLocation, goal))

    @annotation.tailrec
    def aStar(openSet: Set[Node], closedSet: Set[Node]): Option[Node] = {
      if (openSet.isEmpty) None
      else {
        val current = openSet.minBy(_.f)
        val newOpenSet = openSet - current
        val newClosedSet = closedSet + current
        if (current.location == goal) Some(current)
        else {
          val neighbourLocations = getOpenSpaceNeighbours(current.location)
          val neighbourNodes = neighbourLocations map makeNeighbourNode(current)
          def betterNode(nn: Node)(n: Node): Boolean = n.location == nn.location && n.f < nn.f
          val filteredNeighbourNodes = neighbourNodes filter (nn =>
            !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
          aStar(newOpenSet ++ filteredNeighbourNodes, newClosedSet)
        }
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
    winningNode map pathLength
  }

  private def isEven(v: Int): Boolean = v % 2 == 0
}

object Maze {
  def cubicleTypesToStrings(cubicleTypes: Seq[CubicleType.Value], width: Int): Seq[String] =
    (cubicleTypes collect {
      case ct if ct == Wall => "#"
      case ct if ct == OpenSpace => "."
    }).mkString.grouped(width).toList
}
