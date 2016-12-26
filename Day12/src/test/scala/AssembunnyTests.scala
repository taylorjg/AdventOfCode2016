import org.scalatest.FlatSpec

class AssembunnyTests extends FlatSpec {
  "given example" should "give the correct result" in {
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
    val registers = Assembunny.execute(program)
    assert(registers.map("a").value == 42)
  }
}
