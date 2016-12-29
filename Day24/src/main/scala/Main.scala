import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day24-input.txt").getLines.toVector
    val airDuctSpelunking = new AirDuctSpelunking(lines)

//    println(airDuctSpelunking.numberedLocations)

//    val route0to1 = airDuctSpelunking.shortestRoute(0, 1)
//    println(s"route0to1 length: ${route0to1.get.length}")

//    val route0to2 = airDuctSpelunking.shortestRoute(0, 2)
//    println(s"route0to2 length: ${route0to2.get.length}")
//
//    val route0to3 = airDuctSpelunking.shortestRoute(0, 3)
//    println(s"route0to3 length: ${route0to3.get.length}")
//
//    val route1to0 = airDuctSpelunking.shortestRoute(1, 0)
//    println(s"route1to0 length: ${route1to0.get.length}")
//
//    println(s"ShortestRouteCache size: ${airDuctSpelunking.ShortestRouteCache.size}")

      val route0to4 = airDuctSpelunking.shortestRoute(0, 4)
      println(s"route0to4 length: ${route0to4.get.length}")

//    val answer1 = airDuctSpelunking.shortestRoute()
//    println(s"answer1: $answer1")
  }
}
