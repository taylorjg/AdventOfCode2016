object Fred {

  private type Floor = Int
  private type Generator = String
  private type Microchip = String
  private type FloorsToGenerators = Map[Floor, Seq[Generator]]
  private type FloorsToMicrochips= Map[Floor, Seq[Microchip]]
  private type FloorContents = (Floor, Seq[Generator], Seq[Microchip])

  private case class State(elevatorFloor: Floor,
                           m1: FloorsToGenerators,
                           m2: FloorsToMicrochips)

  def process(arrangement: Seq[String]): Int = {
    val parsedLines = arrangement map parseLine
    val initialState = State(1, makeGeneratorsMap(parsedLines), makeMicrochipsMap(parsedLines))
    println(s"initialState: $initialState")
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
    val thing1 = m.group(2)
    val (gs, ms) = parseThing(thing1)
    (FloorNames(floorName), gs, ms)
  }

  private def parseLineContents2(s: String): FloorContents = {
    val m = FloorContains2Regex.findAllIn(s)
    val floorName = m.group(1)
    val thing1 = m.group(2)
    val thing2 = m.group(3)
    val (gs1, ms1) = parseThing(thing1)
    val (gs2, ms2) = parseThing(thing2)
    (FloorNames(floorName), gs1 ++ gs2, ms1 ++ ms2)
  }

  private def parseLineContents3(s: String): FloorContents = {
    val m = FloorContains3Regex.findAllIn(s)
    val floorName = m.group(1)
    val thing1 = m.group(2)
    val thing2 = m.group(3)
    val thing3 = m.group(4)
    val (gs1, ms1) = parseThing(thing1)
    val (gs2, ms2) = parseThing(thing2)
    val (gs3, ms3) = parseThing(thing3)
    (FloorNames(floorName), gs1 ++ gs2 ++ gs3, ms1 ++ ms2 ++ ms3)
  }

  private def parseLineContents4(s: String): FloorContents = {
    val m = FloorContains4Regex.findAllIn(s)
    val floorName = m.group(1)
    val thing1 = m.group(2)
    val thing2 = m.group(3)
    val thing3 = m.group(4)
    val thing4 = m.group(5)
    val (gs1, ms1) = parseThing(thing1)
    val (gs2, ms2) = parseThing(thing2)
    val (gs3, ms3) = parseThing(thing3)
    val (gs4, ms4) = parseThing(thing4)
    (FloorNames(floorName), gs1 ++ gs2 ++ gs3 ++ gs4, ms1 ++ ms2 ++ ms3 ++ ms4)
  }

  private def parseNothingRelevant(s: String): FloorContents = {
    val m = FloorNothingRelevantRegex.findAllIn(s)
    val floorName = m.group(1)
    (FloorNames(floorName), Seq(), Seq())
  }

  private def parseThing(s: String): (Seq[Generator], Seq[Microchip]) = {
    val b1 = GeneratorRegex.pattern.matcher(s).matches
    val b2 = MicrochipRegex.pattern.matcher(s).matches
    (b1, b2) match {
      case (true, false) =>
        val m = GeneratorRegex.findAllIn(s)
        val generator = m.group(1)
        (Seq(generator), Seq())
      case (false, true) =>
        val m = MicrochipRegex.findAllIn(s)
        val microchip = m.group(1)
        (Seq(), Seq(microchip))
      case _ => throw new Exception(s"""Bad input, "$s".""")
    }
  }

  private def makeGeneratorsMap(parsedLines: Seq[FloorContents]): FloorsToGenerators =
    (parsedLines map (pl => (pl._1, pl._2))).toMap

  private def makeMicrochipsMap(parsedLines: Seq[FloorContents]): FloorsToMicrochips =
    (parsedLines map (pl => (pl._1, pl._3))).toMap

  private final val FloorContains1Regex = """The (\w+) floor contains a (\w+(?: generator|-compatible microchip)).""".r
  private final val FloorContains2Regex = """The (\w+) floor contains a (\w+(?: generator|-compatible microchip)) and a (\w+(?: generator|-compatible microchip)).""".r
  private final val FloorContains3Regex = """The (\w+) floor contains a (\w+(?: generator|-compatible microchip)), a (\w+(?: generator|-compatible microchip)), and a (\w+(?: generator|-compatible microchip)).""".r
  private final val FloorContains4Regex = """The (\w+) floor contains a (\w+(?: generator|-compatible microchip)), a (\w+(?: generator|-compatible microchip)), a (\w+(?: generator|-compatible microchip)), and a (\w+(?: generator|-compatible microchip)).""".r
  private final val FloorNothingRelevantRegex = """The (\w+) floor contains nothing relevant.""".r
  private final val GeneratorRegex = """(\w+) generator""".r
  private final val MicrochipRegex = """(\w+)-compatible microchip""".r

  private final val FloorNames = Map(
    "first" -> 1,
    "second" -> 2,
    "third" -> 3,
    "fourth" -> 4)
}
