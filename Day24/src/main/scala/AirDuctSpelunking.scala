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
}
