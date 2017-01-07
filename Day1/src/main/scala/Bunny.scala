object Bunny {

  object Turn extends Enumeration {
    val L, R = Value
  }
  import Turn._

  object Direction extends Enumeration {
    val N, S, E, W = Value
  }
  import Direction._

  implicit class DirectionOps(d: Direction.Value) {
    private final val LeftTurns = Map(N -> W, S -> E, E -> N, W -> S)
    private final val RightTurns = Map(N -> E, S -> W, E -> S, W -> N)
    def turn(t: Turn.Value): Direction.Value = t match {
      case L => LeftTurns(d)
      case R => RightTurns(d)
    }
  }

  type Step = (Turn.Value, Int)
  type Coords = (Int, Int)

  private final val Origin = (0, 0)

  def parseSteps(steps: String): Seq[Step] = {
    def parseStep(step: String): Step = {
      val t = step.head match {
        case 'L' => L
        case 'R' => R
      }
      val n = step.tail.toInt
      (t, n)
    }
    steps split "," map (_.trim) map parseStep
  }

  def distance(steps: Seq[Step]): (Int, Option[Int]) = {

    def getStepLocations(p: Coords, d: Direction.Value, n: Int): List[Coords] = {
      def loop(remaining: Int, l: List[Coords]): List[Coords] =
        if (remaining == 0) l else loop(remaining - 1, travelOne(l.head, d) :: l)
      loop(n, List(p)).reverse drop 1
    }

    def travelOne(p: Coords, d: Direction.Value): Coords = d match {
      case N => (p._1, p._2 + 1)
      case S => (p._1, p._2 - 1)
      case E => (p._1 + 1, p._2)
      case W => (p._1 - 1, p._2)
    }

    def distance(p: Coords, q: Coords): Int =
      Math.abs(p._1 - q._1) + Math.abs(p._2 - q._2)

    val seed = (N, List.empty[Coords], Option.empty[Coords])
    val acc = steps.foldLeft(seed) {
      case ((oldDir, oldLocations, oldRevisitedLocation), (t, n)) =>
        val lastLocation = oldLocations.lastOption getOrElse Origin
        val newDir = oldDir.turn(t)
        val stepLocations = getStepLocations(lastLocation, newDir, n)
        val newLocations = oldLocations ++ stepLocations
        val newRevisitedLocation = oldRevisitedLocation orElse (oldLocations find (stepLocations.contains(_)))
        (newDir, newLocations, newRevisitedLocation)
    }
    val answer1 = distance(Origin, acc._2.last)
    val answer2 = acc._3 map (distance(Origin, _))
    (answer1, answer2)
  }
}
