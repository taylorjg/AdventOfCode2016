import LocationType._

import scala.collection.mutable

class AirDuctSpelunking(lines: Vector[String]) {

  def getLocationType(location: Location): LocationType.Value = {
    if (lines.isDefinedAt(location.y) && lines(location.y).isDefinedAt(location.x))
      lines(location.y)(location.x) match {
        case '#' => Wall
        case _ => OpenSpace
      }
    else Wall
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

  def shortestRoute(): Option[Int] = {
    val startLocation = numberedLocations(0)
    val goals = numberedLocations.values.toSet - startLocation
    val startNode = Node2(path = List(startLocation), goals, lastGoal = startLocation)
    val solutions = findSolutions(Set(startNode), List())
    val sortedSolutions = solutions.sortBy(_.length)
    val shortestSolution = sortedSolutions.headOption
    shortestSolution map (_.length - 1)
  }

  def shortestRoute(fromNumberedLocation: Int, toNumberedLocation: Int): Option[List[Location]] = {
    val fromLocation = numberedLocations(fromNumberedLocation)
    val toLocation = numberedLocations(toNumberedLocation)
    shortestRouteWithCaching(fromLocation, toLocation)
  }

  def shortestRoute(fromLocation: Location, toLocation: Location): Option[List[Location]] = {
    val startNode = Node(fromLocation, None, 0, 0)
    val maybeFinalNode = aStar(toLocation, openSet = Set(startNode), closedSet = Set())
    maybeFinalNode map path
  }

  private final val ShortestRouteCache: mutable.Map[(Location, Location), List[Location]] = scala.collection.mutable.Map[(Location, Location), List[Location]]()

  private def shortestRouteWithCaching(fromLocation: Location, toLocation: Location): Option[List[Location]] = {
    val forwardKey = (fromLocation, toLocation)
    val reverseKey = (toLocation, fromLocation)
    val maybeForwardRoute = ShortestRouteCache.get(forwardKey)
    val maybeReverseRoute = ShortestRouteCache.get(reverseKey)
    val maybeRoute = maybeForwardRoute.orElse(maybeReverseRoute map (_.reverse))
    maybeRoute match {
      case Some(cachedRoute) => Some(cachedRoute)
      case None =>
        println(s"Cache miss $fromLocation to $toLocation")
        shortestRoute(fromLocation, toLocation) match {
          case Some(route) =>
            ShortestRouteCache += (forwardKey -> route)
            ShortestRouteCache += (reverseKey -> route.reverse)
            Some(route)
          case None => None
        }
    }
  }

  def clearShortestRouteCache(): Unit = ShortestRouteCache.clear()

  case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
    val f: Double = g + h
  }

  case class Node2(path: List[Location], goals: Set[Location], lastGoal: Location)

  private def distance(l1: Location, l2: Location): Double = {
    val dx = l1.x - l2.x
    val dy = l1.y - l2.y
    Math.hypot(dx, dy)
  }

  private def getOpenSpaceNeighbourLocations(location: Location): Seq[Location] = {
    val x1 = location.x
    val y1 = location.y
    for {
      (dx, dy) <- Seq((0, -1), (0, 1), (-1, 0), (1, 0))
      (x2, y2) = (x1 + dx, y1 + dy)
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

  private def path(node: Node): List[Location] = {
    @annotation.tailrec
    def loop(n: Node, acc: List[Location]): List[Location] = {
      n.parent match {
        case Some(p) => loop(p, n.location :: acc)
        case None => n.location :: acc
      }
    }
    loop(node, List())
  }

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

        def betterNode(nn: Node)(n: Node): Boolean = n.location == nn.location // && n.f < nn.f

        val filteredNeighbourNodes = neighbourNodes filter (nn =>
          !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
        aStar(goal, newOpenSet ++ filteredNeighbourNodes, newClosedSet)
      }
    }
  }

  @annotation.tailrec
  private def findSolutions(nodes: Set[Node2], solutions: List[List[Location]]): List[List[Location]] = {
    if (nodes.isEmpty) solutions
    else {
      val currentNode = nodes.head
      val newNodes = nodes - currentNode
      if (currentNode.goals.isEmpty) {
        println(s"Found a solution with ${currentNode.path.length - 1} steps")
        findSolutions(newNodes, currentNode.path :: solutions)
      }
      else {
        val newNodes2 = currentNode.goals flatMap (goal => {
          shortestRouteWithCaching(currentNode.lastGoal, goal) match {
            case Some(path) =>
              val newNode = Node2(currentNode.path ++ path.tail, currentNode.goals - goal, goal)
              List(newNode)
            case None =>
              println(s"No route found from ${currentNode.lastGoal} to $goal !!!")
              List()
          }
        })
        findSolutions(newNodes ++ newNodes2, solutions)
      }
    }
  }
}
