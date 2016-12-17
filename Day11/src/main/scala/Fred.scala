object Fred {

  private type Floor = Int
  private type Generator = String
  private type Microchip = String
  private type FloorsToGenerators = Map[Floor, Seq[Generator]]
  private type FloorsToMicrochips= Map[Floor, Seq[Microchip]]
  private type FloorContents = (Floor, Seq[Generator], Seq[Microchip])

  // Commands:
  // ElavatorUp(numFloors, Seq[Generator], Seq[Microchip])
  // ElavatorDown(numFloors, Seq[Generator], Seq[Microchip])

  // Rules:
  // Elevator must contain 1G or 1M or 2G or 2M or 1G+1M
  // ???

  private case class State(elevatorFloor: Floor,
                           m1: FloorsToGenerators,
                           m2: FloorsToMicrochips)

  def process(arrangement: Seq[String]): Int = {
    val parsedLines = arrangement map parseLine
    val initialState = State(1, makeGeneratorsMap(parsedLines), makeMicrochipsMap(parsedLines))
    println(s"initialState: $initialState")
    // Calculate all possible legal sequences of commands that result in all items residing on the 4th floor
    // Find the sequence with the minimum number of steps (i.e. sum of numFloors)
    -1
  }

  private def parseLine(s: String): FloorContents = {
    val b1 = FloorContains1Regex.pattern.matcher(s).matches
    val b2 = FloorContains2Regex.pattern.matcher(s).matches
    val b3 = FloorContains3Regex.pattern.matcher(s).matches
    val b4 = FloorContains4Regex.pattern.matcher(s).matches
    val b5 = FloorNothingRelevantRegex.pattern.matcher(s).matches
    (b1, b2, b3, b4, b5) match {
      case (true, false, false, false, false) => parseLineContents1(s)
      case (false, true, false, false, false) => parseLineContents2(s)
      case (false, false, true, false, false) => parseLineContents3(s)
      case (false, false, false, true, false) => parseLineContents4(s)
      case (false, false, false, false, true) => parseNothingRelevant(s)
      case _ => throw new Exception(s"""Bad input, "$s".""")
    }
  }

  private def parseLineContents1(s: String): FloorContents = {
    val m = FloorContains1Regex.findAllIn(s)
    val floorName = m.group(1)
    val (gs, ms) = parseThing(m.group(2))
    (FloorNames(floorName), gs, ms)
  }

  private def parseLineContents2(s: String): FloorContents = {
    val m = FloorContains2Regex.findAllIn(s)
    val floorName = m.group(1)
    val (gs, ms) = parseThings(m.group(2), m.group(3))
    (FloorNames(floorName), gs, ms)
  }

  private def parseLineContents3(s: String): FloorContents = {
    val m = FloorContains3Regex.findAllIn(s)
    val floorName = m.group(1)
    val (gs, ms) = parseThings(m.group(2), m.group(3), m.group(4))
    (FloorNames(floorName), gs, ms)
  }

  private def parseLineContents4(s: String): FloorContents = {
    val m = FloorContains4Regex.findAllIn(s)
    val floorName = m.group(1)
    val (gs, ms) = parseThings(m.group(2), m.group(3), m.group(4), m.group(5))
    (FloorNames(floorName), gs, ms)
  }

  private def parseNothingRelevant(s: String): FloorContents = {
    val m = FloorNothingRelevantRegex.findAllIn(s)
    val floorName = m.group(1)
    (FloorNames(floorName), Seq(), Seq())
  }

  private def parseThings(things: String*): (Seq[Generator], Seq[Microchip]) =
    things.foldLeft((Seq[Generator](), Seq[Microchip]()))((acc, thing) => {
      val (gs, ms) = parseThing(thing)
      (acc._1 ++ gs, acc._2 ++ ms)
    })

  private def parseThing(thing: String): (Seq[Generator], Seq[Microchip]) = {
    val b1 = GeneratorRegex.pattern.matcher(thing).matches
    val b2 = MicrochipRegex.pattern.matcher(thing).matches
    (b1, b2) match {
      case (true, false) =>
        val m = GeneratorRegex.findAllIn(thing)
        val generator = m.group(1)
        (Seq(generator), Seq())
      case (false, true) =>
        val m = MicrochipRegex.findAllIn(thing)
        val microchip = m.group(1)
        (Seq(), Seq(microchip))
      case _ => throw new Exception(s"""Bad input, "$thing".""")
    }
  }

  private def makeGeneratorsMap(parsedLines: Seq[FloorContents]): FloorsToGenerators =
    (parsedLines map (pl => (pl._1, pl._2))).toMap

  private def makeMicrochipsMap(parsedLines: Seq[FloorContents]): FloorsToMicrochips =
    (parsedLines map (pl => (pl._1, pl._3))).toMap

  private final val F = """\w+"""
  private final val T = """\w+(?: generator|-compatible microchip)"""
  private final val FloorContains1Regex = s"The ($F) floor contains a ($T).".r
  private final val FloorContains2Regex = s"The ($F) floor contains a ($T) and a ($T).".r
  private final val FloorContains3Regex = s"The ($F) floor contains a ($T), a ($T), and a ($T).".r
  private final val FloorContains4Regex = s"The ($F) floor contains a ($T), a ($T), a ($T), and a ($T)."r
  private final val FloorNothingRelevantRegex = s"The ($F) floor contains nothing relevant.".r
  private final val GeneratorRegex = s"($F) generator".r
  private final val MicrochipRegex = s"($F)-compatible microchip".r

  private final val FloorNames = Map(
    "first" -> 1,
    "second" -> 2,
    "third" -> 3,
    "fourth" -> 4)
}
