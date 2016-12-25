import scala.util.Try

object Assembunny {

  case class State(code: Vector[String], registers: Registers, nextInstruction: Int)

  def execute(code: Vector[String], rs: Register*): Registers = {
    @annotation.tailrec
    def loop(state: State): State = if (state.nextInstruction >= code.length) state else loop(executeCommand(state))
    val initialRegisters = new Registers(new Registers().map ++ (rs map (r => (r.name, r))))
    val initialState = State(code, initialRegisters, 0)
    val finalState = loop(initialState)
    finalState.registers
  }

  private def executeCommand(state: State): State = {
    val line = state.code(state.nextInstruction)
    val (newState, maybeJumpIndex) = line match {
      case CpyRegex(x, y) => executeCpyCommand(state, x, y)
      case IncRegex(x) => executeIncCommand(state, x)
      case DecRegex(x) => executeDecCommand(state, x)
      case JnzRegex(x, y) => executeJnzCommand(state, x, y.toInt)
      case TglRegex(x) => executeTglCommand(state, x)
      case _ => throw new Exception(s"""Unknown instruction, "$line".""")
    }
    newState.copy(nextInstruction = maybeJumpIndex.getOrElse(state.nextInstruction + 1))
  }

  private def executeCpyCommand(state: State, x: String, y: String): (State, Option[Int]) = {
    val newValue = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.map(x).value
    }
    val newRegisters = new Registers(state.registers.map.updated(y, Register(y, newValue)))
    (state.copy(registers = newRegisters), None)
  }

  private def executeIncCommand(state: State, x: String): (State, Option[Int]) = {
    val register = state.registers.map(x)
    val newRegisters: Registers = new Registers(state.registers.map.updated(x, Register(x, register.value + 1)))
    (state.copy(registers = newRegisters), None)
  }

  private def executeDecCommand(state: State, x: String): (State, Option[Int]) = {
    val register = state.registers.map(x)
    val newRegisters: Registers = new Registers(state.registers.map.updated(x, Register(x, register.value - 1)))
    (state.copy(registers = newRegisters), None)
  }

  private def executeJnzCommand(state: State, x: String, y: Int): (State, Option[Int]) = {
    val jump = Try(x.toInt).toOption match {
      case Some(v) => v != 0
      case None => state.registers.map(x).value != 0
    }
    val newIndex = if (jump) Some(state.nextInstruction + y) else None
    (state, newIndex)
  }

  private def executeTglCommand(state: State, x: String): (State, Option[Int]) = {
    val newIndex = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.map(x).value
    }
    ???
  }

  private final val CpyRegex = """cpy (\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (\d+|[a-d]) (-?\d+)""".r
  private final val TglRegex = """tgl (\d+|[a-d])""".r
}

// one argument instructions:
//
//   inc  => dec
//
//   dec  => inc
//   tgl  => inc
//   *    => inc
//
// two argument instructions:
//
//   jnz  => cpy
//
//   cpy  => jnz
//   *    => jnz
//
