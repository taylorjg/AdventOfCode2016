import CubicleType._

class Maze(seed: Int) {

  def getLocation(location: (Int, Int)): Cubicle = {
    val (x, y) = location
    val v1 = (x*x) + (3*x) + (2*x*y) + y + (y*y)
    val v2 = v1 + seed
    val v3 = v2.toBinaryString
    val numOnes = v3 count (_ == '1')
    Cubicle(if (isEven(numOnes)) OpenSpace else Wall)
  }

  private def isEven(v: Int): Boolean = v % 2 == 0
}

object Maze {
  def cubiclesToStrings(cubicles: Seq[Cubicle], width: Int): Seq[String] =
    (cubicles collect {
      case c if c.value == Wall => "#"
      case c if c.value == OpenSpace => "."
    }).mkString.grouped(width).toList
}
