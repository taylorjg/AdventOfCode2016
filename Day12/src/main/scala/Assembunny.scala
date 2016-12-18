import scala.util.Try

object Assembunny {

  def execute(code: Vector[String], rs: Register*): Registers = {
    @annotation.tailrec
    def loop(registers: Registers, index: Int): Registers =
      if (index >= code.length) registers
      else {
        val (newRegisters, jumpIndex) = executeCommand(code, registers, index)
        val newIndex = jumpIndex.getOrElse(index + 1)
        loop(newRegisters, newIndex)
      }
    val initialRegisters = new Registers(new Registers().map ++ (rs map (r => (r.name, r))))
    loop(initialRegisters, 0)
  }

  private def executeCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val line = code(index)
    val b1 = CpyRegex.pattern.matcher(line).matches
    val b2 = IncRegex.pattern.matcher(line).matches
    val b3 = DecRegex.pattern.matcher(line).matches
    val b4 = JnzRegex.pattern.matcher(line).matches
    (b1, b2, b3, b4) match {
      case (true, false, false, false) => executeCpyCommand(code, registers, index)
      case (false, true, false, false) => executeIncCommand(code, registers, index)
      case (false, false, true, false) => executeDecCommand(code, registers, index)
      case (false, false, false, true) => executeJnzCommand(code, registers, index)
      case _ => throw new Exception(s"""Unknown instruction, "$line".""")
    }
  }

  private def executeCpyCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = CpyRegex.findAllIn(code(index))
    val x = m.group(1)
    val y = m.group(2)
    val newValue = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => registers.map(x).value
    }
    (new Registers(registers.map.updated(y, Register(y, newValue))), None)
  }

  private def executeIncCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = IncRegex.findAllIn(code(index))
    val x = m.group(1)
    val register = registers.map(x)
    (new Registers(registers.map.updated(x, Register(x, register.value + 1))), None)
  }

  private def executeDecCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = DecRegex.findAllIn(code(index))
    val x = m.group(1)
    val register = registers.map(x)
    (new Registers(registers.map.updated(x, Register(x, register.value - 1))), None)
  }

  private def executeJnzCommand(code: Vector[String], registers: Registers, index: Int): (Registers, Option[Int]) = {
    val m = JnzRegex.findAllIn(code(index))
    val x = m.group(1)
    val y = m.group(2).toInt
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
