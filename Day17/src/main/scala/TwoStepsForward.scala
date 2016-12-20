object TwoStepsForward {

  def shortestPath(passcode: String): Option[String] = {

    val start = Location(0, 0)
    val goal = Location(3, 3)

    case class Node(location: Location, parent: Option[Node], g: Double, h: Double) {
      val f: Double = g + h
    }

    def distance(l1: Location, l2: Location): Double = {
      val dx = l1.x - l2.x
      val dy = l1.y - l2.y
      Math.hypot(dx, dy)
    }

    def getNeighboursWithOpenDoors(node: Node): Seq[Location] = {
      val currentPath = nodeToPath(node)
      val hash = calculateHash(passcode, currentPath)
      val offsets = hashToOffsets(hash)
      val x1 = node.location.x
      val y1 = node.location.y
      for {
        (dx, dy) <- offsets
        (x2, y2) = (x1 + dx, y1 + dy)
        if x2 >= 0 && y2 >= 0
        neighbour = Location(x2, y2)
      } yield neighbour
    }

    def hashToOffsets(hash: String): Seq[(Int, Int)] = {
      def charToLocation(l: (Int, Int), c: Char): Option[(Int, Int)] =
        Some(l).filter(_ => c >= 'b' && c <= 'f')
      Seq(
        charToLocation((0, -1), hash(0)),
        charToLocation((0, 1), hash(1)),
        charToLocation((-1, 0), hash(2)),
        charToLocation((1, 0), hash(3))).flatten
    }

    def makeNeighbourNode(parent: Node)(neighbourLocation: Location): Node =
      Node(
        neighbourLocation,
        Some(parent),
        parent.g + distance(parent.location, neighbourLocation),
        distance(neighbourLocation, goal))

    def nodeToPath(node: Node): String = {
      def loop(n: Node, acc: String): String =
        n.parent match {
          case Some(p) =>
            val direction = nodesToDirection(p, n)
            loop(p, acc + direction)
          case None =>
            acc
        }
      loop(node, "").reverse
    }

    def nodesToDirection(fromNode: Node, toNode: Node): String = {
      val dx = toNode.location.x - fromNode.location.x
      val dy = toNode.location.y - fromNode.location.y
      (dx, dy) match {
        case (0, -1) => "U"
        case (0, 1) => "D"
        case (-1, 0) => "L"
        case (1, 0) => "R"
        case _ => throw new Exception("Error in nodesToDirection")
      }
    }

    @annotation.tailrec
    def aStar(openSet: Set[Node], closedSet: Set[Node]): Option[Node] = {
      if (openSet.isEmpty) None
      else {
        val current = openSet.minBy(_.f)
        val newOpenSet = openSet - current
        val newClosedSet = closedSet + current
        if (current.location == goal) Some(current)
        else {
          val neighbourLocations = getNeighboursWithOpenDoors(current)
          val neighbourNodes = neighbourLocations map makeNeighbourNode(current)
          def betterNode(nn: Node)(n: Node): Boolean = n.location == nn.location && n.f < nn.f
          val filteredNeighbourNodes = neighbourNodes filter (nn =>
            !newOpenSet.exists(betterNode(nn)) && !newClosedSet.exists(betterNode(nn)))
          aStar(newOpenSet ++ filteredNeighbourNodes, newClosedSet)
        }
      }
    }

    val startNode = Node(start, None, 0, 0)
    val winningNode = aStar(openSet = Set(startNode), closedSet = Set())
    winningNode map nodeToPath
  }

  private final val MessageDigest = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(passcode: String, currentPath: String): String = {
    val bytes = s"$passcode$currentPath".getBytes("UTF-8")
    MessageDigest.update(bytes, 0, bytes.length)
    MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
