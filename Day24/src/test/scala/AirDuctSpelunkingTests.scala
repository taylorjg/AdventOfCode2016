import org.scalatest.FlatSpec
import LocationType._

class AirDuctSpelunkingTests extends FlatSpec {

  private final val Input =
    """
      |###########
      |#0.1.....2#
      |#.#######.#
      |#4.......3#
      |###########
    """.stripMargin

  private final val Lines = Input.split("\n").map(_.trim).filter(_.nonEmpty).toVector

  "identifying walls and open spaces" should "work correctly" in {
    val airDuctSpelunking = new AirDuctSpelunking(Lines)
    assert(airDuctSpelunking.getLocationType(Location(0, 0)) == Wall)
    assert(airDuctSpelunking.getLocationType(Location(2, 2)) == Wall)
    assert(airDuctSpelunking.getLocationType(Location(2, 1)) == OpenSpace)
    assert(airDuctSpelunking.getLocationType(Location(1, 1)) == OpenSpace)
  }

  "finding numbered locations" should "work correctly" in {
    val airDuctSpelunking = new AirDuctSpelunking(Lines)
    assert(airDuctSpelunking.numberedLocations == Map(
      0 -> Location(1, 1),
      1 -> Location(3, 1),
      2 -> Location(9, 1),
      3 -> Location(9, 3),
      4 -> Location(1, 3)))
  }

  "shortest route from 4 to 1" should "be 4 steps" in {
    val airDuctSpelunking = new AirDuctSpelunking(Lines)
    assert(airDuctSpelunking.shortestRoute(4, 1).contains(4))
  }

  "given example" should "find the shortest route" in {
    val airDuctSpelunking = new AirDuctSpelunking(Lines)
    assert(airDuctSpelunking.shortestRoute().contains(14))
  }
}
