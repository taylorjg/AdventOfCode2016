import LocationType._

class AirDuctSpelunking(lines: Vector[String]) {

  def getLocationType(location: Location): LocationType.Value =
    lines(location.y)(location.x) match {
      case '#' => Wall
      case _ => OpenSpace
    }

  val numberedLocations: Map[Int, Location] = {
    val maxX = lines(0).indices.last
    val maxY = lines.indices.last
    val kvps = for {
      x <- 0 to maxX
      y <- 0 to maxY
      ch = lines(y)(x)
      if ch >= '0' && ch <= '9'
      location = Location(x, y)
      number = ch - '0'
    } yield number -> location
    kvps.toMap
  }

  def shortestRoute(): Int = {
    -1
  }

  def shortestRoute(fromNumberedLocation: Int, toNumberedLocation: Int): Option[Int] = {
    val start = numberedLocations(fromNumberedLocation)
    val goal = numberedLocations(toNumberedLocation)
    val startNode = Node(start, None, 0, 0)
    val maybeGoalNode = aStar(goal, openSet = Set(startNode), closedSet = Set())
    maybeGoalNode map pathLength
  }
  case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
    val f = g + h
  }

  private def distance(l1: Location, l2: Location): Double = {
    val dx = l1.x - l2.x
    val dy = l1.y - l2.y
    Math.hypot(dx, dy)
  }

  private def getOpenSpaceNeighbourLocations(location: Location): Seq[Location] = {
    val x1 = location.x
    val y1 = location.y
    for {
      (dx, dy) <- Seq((0, 1), (0, -1), (-1, 0), (1, 0))
      (x2, y2) = (x1 + dx, y1 + dy)
      if x2 >= 0 && y2 >= 0
      neighbour = Location(x2, y2)
      if getLocationType(neighbour) == OpenSpace
    } yield neighbour
  }

  private def makeNeighbourNode(parent: Node, goal: Location)(location: Location): Node =
    Node(
      location,
      Some(parent),
      parent.g + distance(parent.location, location),
      distance(location, goal))

  @annotation.tailrec
  private def aStar(goal: Location, openSet: Set[Node], closedSet: Set[Node]): Option[Node] = {
    if (openSet.isEmpty) None
    else {
      val currentNode = openSet.minBy(_.f)
      val newOpenSet = openSet - currentNode
      val newClosedSet = closedSet + currentNode
      if (currentNode.location == goal) Some(currentNode)
      else {
        val neighbourLocations = getOpenSpaceNeighbourLocations(currentNode.location)
        val neighbourNodes = neighbourLocations map makeNeighbourNode(currentNode, goal)
        def betterNode(nn: Node)(n: Node): Boolean = n.location == nn.location && n.f < nn.f
        val filteredNeighbourNodes = neighbourNodes filter (nn =>
          !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
        aStar(goal, newOpenSet ++ filteredNeighbourNodes, newClosedSet)
      }
    }
  }

  private def pathLength(node: Node): Int = {
    @annotation.tailrec
    def loop(n: Node, acc: Int): Int =
      n.parent match {
        case Some(child) => loop(child, acc + 1)
        case None => acc
      }
    loop(node, 0)
  }
}
