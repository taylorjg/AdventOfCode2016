import org.scalatest.FlatSpec

class BunnyTests extends FlatSpec {

  "R2, L3" should "be 5 blocks away" in {
    val steps = Bunny.parseSteps("R2, L3")
    assert(Bunny.distance(steps) == (5, None))
  }

  "R2, R2, R2" should "be 2 blocks away" in {
    val steps = Bunny.parseSteps("R2, R2, R2")
    assert(Bunny.distance(steps) == (2, None))
  }

  "R5, L5, R5, R3" should "be 12 blocks away" in {
    val steps = Bunny.parseSteps("R5, L5, R5, R3")
    assert(Bunny.distance(steps) == (12, None))
  }

  "R8, R4, R4, R8" should "be 8 blocks away with first revisited location at 4 blocks away" in {
    val steps = Bunny.parseSteps("R8, R4, R4, R8")
    assert(Bunny.distance(steps) == (8, Some(4)))
  }
}
