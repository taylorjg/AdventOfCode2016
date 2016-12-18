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
    val code = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val registers = Assembunny.execute(code.toVector)
    assert(registers.map("a").value == 42)
  }
}
