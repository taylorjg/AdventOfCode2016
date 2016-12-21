import org.scalatest.FlatSpec

class TwoStepsForwardTests extends FlatSpec {

  "passcode ihgpwlah" should "have a shortest path of DDRRRD" in {
    assert(TwoStepsForward.shortestPath("ihgpwlah").contains("DDRRRD"))
  }

  "passcode kglvqrro" should "have a shortest path of DDUDRLRRUDRD" in {
    assert(TwoStepsForward.shortestPath("kglvqrro").contains("DDUDRLRRUDRD"))
  }

  "passcode ulqzkmiv" should "have a shortest path of DRURDRUDDLLDLUURRDULRLDUUDDDRR" in {
    assert(TwoStepsForward.shortestPath("ulqzkmiv").contains("DRURDRUDDLLDLUURRDULRLDUUDDDRR"))
  }

  "passcode ihgpwlah" should "have a longest path that takes 370 steps" in {
    assert(TwoStepsForward.longestPathNumSteps("ihgpwlah").contains(370))
  }

  "passcode kglvqrro" should "have a longest path that takes 492 steps" in {
    assert(TwoStepsForward.longestPathNumSteps("kglvqrro").contains(492))
  }

  "passcode ulqzkmiv" should "have a longest path that takes 830 steps" in {
    assert(TwoStepsForward.longestPathNumSteps("ulqzkmiv").contains(830))
  }
}
