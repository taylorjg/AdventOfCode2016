import scala.util.Try

object Assembunny {

  def execute(code: Vector[String]): Registers =
    code.indices.foldLeft((new Registers(), Option.empty[Int])) {
      case (acc @ (registers, jumpIndex), index) => {
        val newIndex = jumpIndex.getOrElse(index)
        if (newIndex >= code.length) acc
        else executeCommand(code, registers, newIndex)
      }
    }._1

  private def executeCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val line = code(index)
    val b1 = CpyRegex.pattern.matcher(line).matches
    val b2 = IncRegex.pattern.matcher(line).matches
    val b3 = DecRegex.pattern.matcher(line).matches
    val b4 = JnzRegex.pattern.matcher(line).matches
    val result = (b1, b2, b3, b4) match {
      case (true, false, false, false) => executeCpyCommand(code, registers, index)
      case (false, true, false, false) => executeIncCommand(code, registers, index)
      case (false, false, true, false) => executeDecCommand(code, registers, index)
      case (false, false, false, true) => executeJnzCommand(code, registers, index)
      case _ => throw new Exception(s"""Unknown instruction, "$line".""")
    }
    println(s"result: ${result._1.map}; ${result._2}")
    result
  }

  private def executeCpyCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = CpyRegex.findAllIn(code(index))
    val x = m.group(1)
    val y = m.group(2)
    println(s"x: $x; y: $y")
    val newValue = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => registers.map(x).value
    }
    (new Registers(registers.map.updated(y, Register(y, newValue))), None)
  }

  private def executeIncCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = IncRegex.findAllIn(code(index))
    val x = m.group(1)
    println(s"x: $x")
    val register = registers.map(x)
    (new Registers(registers.map.updated(x, Register(x, register.value + 1))), None)
  }

  private def executeDecCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = DecRegex.findAllIn(code(index))
    val x = m.group(1)
    println(s"x: $x")
    val register = registers.map(x)
    (new Registers(registers.map.updated(x, Register(x, register.value - 1))), None)
  }

  private def executeJnzCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = JnzRegex.findAllIn(code(index))
    println(s"m: $m")
    val x = m.group(1)
    val y = m.group(2).toInt
    println(s"x: $x; y: $y")
    val jump = Try(x.toInt).toOption match {
      case Some(v) => v != 0
      case None => registers.map(x).value != 0
    }
    val newIndex = if (jump) Some(index + y) else None
    (registers, newIndex)
  }

  private final val CpyRegex = """cpy (\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (\d+|[a-d]) (-?\d+)""".r
}
