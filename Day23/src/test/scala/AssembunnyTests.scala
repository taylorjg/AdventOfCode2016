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
    val code = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val registers = Assembunny.execute(code.toVector)
    assert(registers.map("a").value == 42)
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
    val code = input.split("\n") map (_.trim) filter (_.nonEmpty)
    val registers = Assembunny.execute(code.toVector)
    assert(registers.map("a").value == 3)
  }
}
