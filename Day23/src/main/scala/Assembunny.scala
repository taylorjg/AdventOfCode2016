import scala.util.Try

object Assembunny {

  case class State(program: Vector[Instruction], registers: Registers, nextInstruction: Int)

  sealed trait Instruction {
    self: NumArguments =>
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
    val (newState, maybeJumpOffset) = instruction match {
      case Cpy(x, y) => executeCpyInstruction(state, x, y)
      case Inc(x) => executeIncInstruction(state, x)
      case Dec(x) => executeDecInstruction(state, x)
      case Jnz(x, y) => executeJnzInstruction(state, x, y)
      case Tgl(x) => executeTglInstruction(state, x)
    }
    newState.copy(nextInstruction = state.nextInstruction + maybeJumpOffset.getOrElse(1))
  }

  private def executeCpyInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    println(s"[executeCpyInstruction] x: $x; y: $y")
    val newValue = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(x)
    }
    Try(y.toInt).toOption match {
      case Some(_) =>
        (state, None)
      case None =>
        val newRegisters = state.registers.setValue(y, newValue)
        (state.copy(registers = newRegisters), None)
    }
  }

  private def executeIncInstruction(state: State, x: String): (State, Option[Int]) = {
    Try(x.toInt).toOption match {
      case Some(_) =>
        (state, None)
      case None =>
        val r = state.registers.map(x)
        val newValue = r.value + 1
        val newRegisters = state.registers.setValue(x, newValue)
        (state.copy(registers = newRegisters), None)
    }
  }

  private def executeDecInstruction(state: State, x: String): (State, Option[Int]) = {
    Try(x.toInt).toOption match {
      case Some(_) =>
        (state, None)
      case None =>
        val r = state.registers.map(x)
        val newValue = r.value - 1
        val newRegisters = state.registers.setValue(x, newValue)
        (state.copy(registers = newRegisters), None)
    }
  }

  private def executeJnzInstruction(state: State, x: String, y: String): (State, Option[Int]) = {
    val jump = Try(x.toInt).toOption match {
      case Some(v) => v != 0
      case None => state.registers.getValue(x) != 0
    }
    val offset = Try(y.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(y)
    }
    val maybeJumpOffset = if (jump) Some(offset) else None
    (state, maybeJumpOffset)
  }

  private def executeTglInstruction(state: State, x: String): (State, Option[Int]) = {
    val offset = Try(x.toInt).toOption match {
      case Some(v) => v
      case None => state.registers.getValue(x)
    }
    val (newProgram, maybeJumpIndex) = toggleInstruction(state, offset)
    (state.copy(program = newProgram), maybeJumpIndex)
  }

  private def toggleInstruction(state: State, offset: Int): (Vector[Instruction], Option[Int]) = {
    val targetInstructionIndex = state.nextInstruction + offset
    if (!state.program.isDefinedAt(targetInstructionIndex)) (state.program, None)
    else {
      val newInstruction = state.program(targetInstructionIndex) match {
        case Cpy(x, y) => Jnz(x, y)
        case Inc(x) => Dec(x)
        case Dec(x) => Inc(x)
        case Jnz(x, y) => Cpy(x, y)
        case Tgl(x) => Inc(x)
      }
      val newInstructions = state.program.zipWithIndex.map {
        case (oldInstruction, index) => if (index == targetInstructionIndex) newInstruction else oldInstruction
      }
      println(s"old instructions:")
      state.program foreach println
      println(s"new instructions:")
      newInstructions foreach println
      (newInstructions, None)
    }
  }

  private final val CpyRegex = """cpy (-?\d+|[a-d]) ([a-d])""".r
  private final val IncRegex = """inc ([a-d])""".r
  private final val DecRegex = """dec ([a-d])""".r
  private final val JnzRegex = """jnz (-?\d+|[a-d]) (-?\d+|[a-d])""".r
  private final val TglRegex = """tgl (-?\d+|[a-d])""".r
}
