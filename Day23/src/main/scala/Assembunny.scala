import scala.util.Try

object Assembunny {

  case class State(program: Vector[Instruction], registers: Registers, instructionPointer: Int)

  sealed trait Instruction
  case class Cpy(x: String, y: String) extends Instruction
  case class Inc(x: String) extends Instruction
  case class Dec(x: String) extends Instruction
  case class Jnz(x: String, y: String) extends Instruction
  case class Tgl(x: String) extends Instruction
  case class Mul(x: String, y: String) extends Instruction

  def parseProgram(program: Seq[String]): Vector[Instruction] = {
    def parseLine(line: String): Instruction = {
      line match {
        case CpyRegex(x, y) => Cpy(x, y)
        case IncRegex(x) => Inc(x)
        case DecRegex(x) => Dec(x)
        case JnzRegex(x, y) => Jnz(x, y)
        case TglRegex(x) => Tgl(x)
        case MulRegex(x, y) => Mul(x, y)
        case _ => throw new Exception(s"""Don't know how to parse "$line".""")
      }
    }
    program
      .map(_.trim)
      .filter(_.nonEmpty)
      .filter(!_.startsWith(LineComment))
      .map(parseLine).toVector
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

  private final val LineComment = ";"
  private final val NextInstructionOffset = 1

  private def executeCommand(state: State): State = {
    val instruction = state.program(state.instructionPointer)
    val (newState, maybeJumpOffset) = instruction match {
      case Cpy(x, y) => executeCpyInstruction(state, x, y)
      case Inc(x) => executeIncInstruction(state, x)
      case Dec(x) => executeDecInstruction(state, x)
      case Jnz(x, y) => executeJnzInstruction(state, x, y)
      case Tgl(x) => executeTglInstruction(state, x)
      case Mul(x, y) => executeMulInstruction(state, x, y)
    }
    val newInstructionPointer = state.instructionPointer + maybeJumpOffset.getOrElse(NextInstructionOffset)
    newState.copy(instructionPointer = newInstructionPointer)
  }

  private def executeCpyInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    valueOfRegisterOrSkipInstruction(state, y, _ => {
      val newValue = valueOfLiteralOrRegister(state, x)
      val newRegisters = state.registers.setValue(y, newValue)
      (state.copy(registers = newRegisters), None)
    })
  }

  private def executeIncInstruction(state: State, x: String): (State, Option[Int]) = {
    valueOfRegisterOrSkipInstruction(state, x, value => {
      val newValue = value + 1
      val newRegisters = state.registers.setValue(x, newValue)
      (state.copy(registers = newRegisters), None)
    })
  }

  private def executeDecInstruction(state: State, x: String): (State, Option[Int]) = {
    valueOfRegisterOrSkipInstruction(state, x, value => {
      val newValue = value - 1
      val newRegisters = state.registers.setValue(x, newValue)
      (state.copy(registers = newRegisters), None)
    })
  }

  private def executeJnzInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val nz = valueOfLiteralOrRegister(state, x) != 0
    val jumpOffset = valueOfLiteralOrRegister(state, y)
    val maybeJumpOffset = Some(jumpOffset) filter (_ => nz)
    (state, maybeJumpOffset)
  }

  private def executeTglInstruction(state: State, x: String): (State, Option[Int]) = {
    val targetInstructionOffset = valueOfLiteralOrRegister(state, x)
    val newProgram = toggleInstruction(state, targetInstructionOffset)
    (state.copy(program = newProgram), None)
  }

  private def executeMulInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val aValue = state.registers.getValue("a")
    val xValue = state.registers.getValue(x)
    val yValue = state.registers.getValue(y)
    val newRegisters = state.registers.setValue("a", aValue + xValue * yValue)
    (state.copy(registers = newRegisters), None)
  }

  private def toggleInstruction(state: State, offset: Int): Vector[Instruction] = {
    val targetInstructionIndex = state.instructionPointer + offset
    if (!state.program.isDefinedAt(targetInstructionIndex)) state.program
    else {
      val newInstruction = state.program(targetInstructionIndex) match {
        case Cpy(x, y) => Jnz(x, y)
        case Inc(x) => Dec(x)
        case Dec(x) => Inc(x)
        case Jnz(x, y) => Cpy(x, y)
        case Tgl(x) => Inc(x)
        case Mul(x, y) => Jnz(x, y)
      }
      val newInstructions = state.program.zipWithIndex.map {
        case (oldInstruction, index) => if (index == targetInstructionIndex) newInstruction else oldInstruction
      }
      newInstructions
    }
  }

  private def valueOfLiteralOrRegister(state: State, s: String): Int =
    Try(s.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(s)
    }

  private def valueOfRegisterOrSkipInstruction(state: State, s: String, f: Int => (State, Option[Int])): (State, Option[Int]) =
    Try(s.toInt).toOption match {
      case Some(_) => (state, None)
      case None => f(state.registers.getValue(s))
    }

  private final val CpyRegex = """cpy (-?\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (-?\d+|[a-d]) (-?\d+|[a-d])""".r
  private final val TglRegex = """tgl (-?\d+|[a-d])""".r
  private final val MulRegex = """mul ([a-d]) ([a-d])""".r
}
