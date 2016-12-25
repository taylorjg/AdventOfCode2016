import scala.util.Try

object Assembunny {

  case class State(program: Vector[Instruction], registers: Registers, nextInstruction: Int)

  sealed trait Instruction {
    self: NumArguments =>
    {
    }
  }

  sealed trait NumArguments {
    val numArguments: Int
  }

  sealed trait OneArgument extends NumArguments {
    val numArguments = 1
  }

  sealed trait TwoArguments extends NumArguments {
    val numArguments = 2
  }

  case class Cpy(x: String, y: String) extends Instruction with TwoArguments

  case class Inc(x: String) extends Instruction with OneArgument

  case class Dec(x: String) extends Instruction with OneArgument

  case class Jnz(x: String, y: String) extends Instruction with TwoArguments

  case class Tgl(x: String) extends Instruction with OneArgument

  def parseProgram(program: Seq[String]): Vector[Instruction] = {
    def parseLine(line: String): Instruction = {
      line match {
        case CpyRegex(x, y) => Cpy(x, y)
        case IncRegex(x) => Inc(x)
        case DecRegex(x) => Dec(x)
        case JnzRegex(x, y) => Jnz(x, y)
        case TglRegex(x) => Tgl(x)
        case _ => throw new Exception(s"""Don't know how to parse "$line".""")
      }
    }
    program.map(parseLine).toVector
  }

  def execute(program: Vector[Instruction], rs: Register*): Registers = {
    @annotation.tailrec
    def loop(state: State): State =
      if (state.nextInstruction >= program.length) state else loop(executeCommand(state))
    val initialRegisters = new Registers(rs)
    val initialState = State(program, initialRegisters, 0)
    val finalState = loop(initialState)
    finalState.registers
  }

  private def executeCommand(state: State): State = {
    val instruction = state.program(state.nextInstruction)
    val (newState, maybeJumpTo) = instruction match {
      case Cpy(x, y) => executeCpyInstruction(state, x, y)
      case Inc(x) => executeIncInstruction(state, x)
      case Dec(x) => executeDecInstruction(state, x)
      case Jnz(x, y) => executeJnzInstruction(state, x, y)
      case Tgl(x) => executeTglInstruction(state, x)
    }
    newState.copy(nextInstruction = maybeJumpTo.getOrElse(state.nextInstruction + 1))
  }

  private def executeCpyInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val newValue = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(x)
    }
    val newRegisters = state.registers.setValue(y, newValue)
    (state.copy(registers = newRegisters), None)
  }

  private def executeIncInstruction(state: State, x: String): (State, Option[Int]) = {
    val r = state.registers.map(x)
    val newRegisters = state.registers.setValue(x, r.value + 1)
    (state.copy(registers = newRegisters), None)
  }

  private def executeDecInstruction(state: State, x: String): (State, Option[Int]) = {
    val r = state.registers.map(x)
    val newRegisters = state.registers.setValue(x, r.value - 1)
    (state.copy(registers = newRegisters), None)
  }

  private def executeJnzInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val jump = Try(x.toInt).toOption match {
      case Some(v) => v != 0
      case None => state.registers.getValue(x) != 0
    }
    val newIndex = if (jump) Some(state.nextInstruction + y.toInt) else None
    (state, newIndex)
  }

  private def executeTglInstruction(state: State, x: String): (State, Option[Int]) = {
    val newIndex = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(x)
    }
    ???
  }

  private final val CpyRegex = """cpy (\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (\d+|[a-d]) (-?\d+)""".r
  private final val TglRegex = """tgl (\d+|[a-d])""".r
}
