import scala.util.Try

object Assembunny {

  case class State(program: Vector[Instruction], registers: Registers, instructionPointer: Int)

  sealed trait Instruction
  case class Cpy(x: String, y: String) extends Instruction
  case class Inc(x: String) extends Instruction
  case class Dec(x: String) extends Instruction
  case class Jnz(x: String, y: String) extends Instruction

  def parseProgram(program: Seq[String]): Vector[Instruction] = {
    def parseLine(line: String): Instruction = {
      line match {
        case CpyRegex(x, y) => Cpy(x, y)
        case IncRegex(x) => Inc(x)
        case DecRegex(x) => Dec(x)
        case JnzRegex(x, y) => Jnz(x, y)
        case _ => throw new Exception(s"""Don't know how to parse "$line".""")
      }
    }
    program.map(parseLine).toVector
  }

  def execute(program: Vector[Instruction], rs: Register*): Registers = {
    @annotation.tailrec
    def loop(state: State): State =
      if (state.instructionPointer >= program.length) state else loop(executeCommand(state))
    val initialRegisters = new Registers(rs)
    val initialState = State(program, initialRegisters, 0)
    val finalState = loop(initialState)
    finalState.registers
  }

  private final val NextInstructionOffset = 1

  private def executeCommand(state: State): State = {
    val instruction = state.program(state.instructionPointer)
    val (newState, maybeJumpOffset) = instruction match {
      case Cpy(x, y) => executeCpyInstruction(state, x, y)
      case Inc(x) => executeIncInstruction(state, x)
      case Dec(x) => executeDecInstruction(state, x)
      case Jnz(x, y) => executeJnzInstruction(state, x, y)
    }
    val newInstructionPointer = state.instructionPointer + maybeJumpOffset.getOrElse(NextInstructionOffset)
    newState.copy(instructionPointer = newInstructionPointer)
  }

  private def executeCpyInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val value = valueOfLiteralOrRegister(state, x)
    val newRegisters = state.registers.setValue(y, value)
    (state.copy(registers = newRegisters), None)
  }

  private def executeIncInstruction(state: State, x: String): (State, Option[Int]) = {
    val value = valueOfRegister(state, x)
    val newRegisters = state.registers.setValue(x, value + 1)
    (state.copy(registers = newRegisters), None)
  }

  private def executeDecInstruction(state: State, x: String): (State, Option[Int]) = {
    val value = valueOfRegister(state, x)
    val newRegisters = state.registers.setValue(x, value - 1)
    (state.copy(registers = newRegisters), None)
  }

  private def executeJnzInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val nz = valueOfLiteralOrRegister(state, x) != 0
    val jumpOffset = valueOfLiteralOrRegister(state, y)
    val maybeJumpOffset = Some(jumpOffset) filter (_ => nz)
    (state, maybeJumpOffset)
  }

  private def valueOfRegister(state: State, s: String): Int =
    state.registers.getValue(s)

  private def valueOfLiteralOrRegister(state: State, s: String): Int =
    Try(s.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(s)
    }

  private final val CpyRegex = """cpy (-?\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (-?\d+|[a-d]) (-?\d+|[a-d])""".r
}
