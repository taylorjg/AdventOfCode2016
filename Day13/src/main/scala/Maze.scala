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
  def path(start: Location, goal: Location): Seq[Step] = {

    private case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
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
        dx <- -1 to 1
        dy <- -1 to 1
        x2 = x1 + dx
        y2 = y1 + dy
        if x2 >= 0 && y2 >= 0
        neighbour = Location(x2, y2)
        if neighbour != location
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

      if (openSet.isEmpty) throw new Exception("BOOM") // or make return type Option[Node] and return None

      val current = openSet.minBy(_.f)
      val newOpenSet = openSet - current
      val newClosedSet = closedSet + current
      if (current.location == goal) current
      else {
        val neighbourLocations = getOpenSpaceNeighbours(current.location)
        val neighbourNodes = neighbourLocations map makeNeighbourNode(current)
        val filteredNeighbourNodes = neighbourNodes filter (nn =>
          !newOpenSet.exists(n => ???) && !newClosedSet.exists(n => ???))
        aStar(newOpenSet ++ filteredNeighbourNodes, newClosedSet)
      }
    }

    val startNode = Node(start, None, 0, 0)
    val winningNode = aStar(openSet = Set(startNode), closedSet = Set())
    println(s"winningNode: $winningNode")

    Seq()
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
