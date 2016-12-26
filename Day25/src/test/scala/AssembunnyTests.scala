import org.scalatest.FlatSpec

class AssembunnyTests extends FlatSpec {

  "old given example" should "give the correct result" in {
    val input =
      """
        |cpy 41 a
        |inc a
        |inc a
        |dec a
        |jnz a 2
        |dec a
      """.stripMargin
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val program = Assembunny.parseProgram(lines)
    val finalState = Assembunny.execute(program)
    assert(finalState.registers.a == 42)
  }

  "new given example" should "give the correct result" in {
    val input =
      """
        |cpy 2 a
        |tgl a
        |tgl a
        |tgl a
        |cpy 1 a
        |dec a
        |dec a
      """.stripMargin
    val lines = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val program = Assembunny.parseProgram(lines)
    val finalState = Assembunny.execute(program)
    assert(finalState.registers.a == 3)
  }
}
