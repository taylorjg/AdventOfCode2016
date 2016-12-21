object TwoStepsForward {

  private object PathType extends Enumeration {
    val Shortest, Longest = Value
  }
  import PathType._

  def shortestPath(passcode: String): Option[String] =
    findPath(passcode, Shortest)

  def longestPathNumSteps(passcode: String): Option[Int] =
    findPath(passcode, Longest) map (_.length)

  private def findPath(passcode: String, pathType: PathType.Value): Option[String] = {

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
      val directionsAndOffsets = hashToDirectionsAndOffsets(hash)
      val offsets = directionsAndOffsets map (_._2)
      val x1 = node.location.x
      val y1 = node.location.y
      for {
        (dx, dy) <- offsets
        (x2, y2) = (x1 + dx, y1 + dy)
        if x2 >= 0 && y2 >= 0 && x2 < 4 && y2 < 4
        neighbour = Location(x2, y2)
      } yield neighbour
    }

    def hashToDirectionsAndOffsets(hash: String): Seq[(String, (Int, Int))] = {
      def charToLocation(d: String, o: (Int, Int), c: Char): Option[(String, (Int, Int))] =
        Some((d, o)).filter(_ => c >= 'b' && c <= 'f')
      Seq(
        charToLocation("U", (0, -1), hash(0)),
        charToLocation("D", (0, 1), hash(1)),
        charToLocation("L", (-1, 0), hash(2)),
        charToLocation("R", (1, 0), hash(3))).flatten
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
    def aStar(openSet: Set[Node], closedSet: Set[Node], lastKnownGood: Option[Node]): Option[Node] = {
      if (openSet.isEmpty) lastKnownGood
      else {
        val current = openSet.minBy(_.f)
        val newOpenSet = openSet - current
        val newClosedSet = closedSet + current
        if (current.location == goal) {
          if (pathType == Shortest) Some(current)
          else aStar(newOpenSet, newClosedSet, Some(current))
        }
        else {
          val neighbourLocations = getNeighboursWithOpenDoors(current)
          val neighbourNodes = neighbourLocations map makeNeighbourNode(current)
          aStar(newOpenSet ++ neighbourNodes, newClosedSet, lastKnownGood)
        }
      }
    }

    val startNode = Node(start, None, 0, 0)
    val winningNode = aStar(openSet = Set(startNode), closedSet = Set(), lastKnownGood = None)
    winningNode map nodeToPath
  }

  private final val MessageDigest = java.security.MessageDigest.getInstance("MD5")

  private def calculateHash(passcode: String, currentPath: String): String = {
    val bytes = s"$passcode$currentPath".getBytes("UTF-8")
    MessageDigest.update(bytes, 0, bytes.length)
    MessageDigest.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }
}
