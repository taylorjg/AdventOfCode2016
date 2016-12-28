import LocationType._

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
    val startNode = Node(startLocation, None, 0, 0, goals, None, List())
    val finalNodes = aStar2(openSet = Set(startNode), closedSet = Set(), solutions = List())
    //finalNodes.map(pathLength).sortBy(identity).headOption
    val v1 =finalNodes.sortBy(pathLength).headOption
    v1 foreach { fn =>
      def loop(n: Node, acc: List[Location]): List[Location] = {
        n.parent match {
          case Some(p) => loop(p, n.location :: acc)
          case None => n.location :: acc
        }
      }
      println(loop(fn, List()).mkString(" => "))
    }
    v1.map(pathLength)
  }

  def shortestRoute(fromNumberedLocation: Int, toNumberedLocation: Int): Option[Int] = {
    val start = numberedLocations(fromNumberedLocation)
    val goal = numberedLocations(toNumberedLocation)
    val startNode = Node(start, None, 0, 0, Set(), None, List())
    val maybeGoalNode = aStar(goal, openSet = Set(startNode), closedSet = Set())
    maybeGoalNode map pathLength
  }

  case class Node(location: Location, parent: Option[Node], g: Double, h: Double, goals: Set[Location], goal: Option[Location], goalsPath: List[Location]) {
    val f: Double = g + h
    override def toString: String = s"[Node] location: $location; goals: $goals; goal: $goal"
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
      distance(location, goal),
      parent.goals,
      parent.goal,
      parent.goalsPath)

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

  @annotation.tailrec
  private def aStar2(openSet: Set[Node], closedSet: Set[Node], solutions: List[Node]): List[Node] = {
    if (openSet.isEmpty) solutions
    else {
      val currentNode = openSet.minBy(_.f)
      println(s"currentNode: $currentNode")
      val newOpenSet = openSet - currentNode
      val newClosedSet = closedSet + currentNode
      if (currentNode.goal.contains(currentNode.location)) {
        if (currentNode.goals.isEmpty) {
          aStar2(newOpenSet, newClosedSet, currentNode :: solutions)
        }
        else {
          println(s"Reached goal at location ${currentNode.goal.get}")
          val newNode = Node(currentNode.location, currentNode.parent, 0, distance(currentNode.location, currentNode.goal.get), currentNode.goals, None, currentNode.goalsPath)
          aStar2(newOpenSet + newNode, newClosedSet, solutions)
        }
      }
      else {
        val newNodes = currentNode.goal match {
          case Some(goal) =>
            val newLocations = getOpenSpaceNeighbourLocations(currentNode.location)
            val v1 = newLocations map makeNeighbourNode(currentNode, goal)
            def betterNode(nn: Node)(n: Node): Boolean = n.goal.isDefined && n.location == nn.location && n.goal == nn.goal && n.goalsPath == nn.goalsPath && n.f < nn.f
            val v2 = v1 filter (nn => !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
            v2
          case None =>
            currentNode.goals map (goal => {
              val remainingGoals = currentNode.goals - goal
              Node(currentNode.location, currentNode.parent, 0, 0, remainingGoals, Some(goal), currentNode.goalsPath ++ List(goal))
            })
        }
        aStar2(newOpenSet ++ newNodes, newClosedSet, solutions)
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
