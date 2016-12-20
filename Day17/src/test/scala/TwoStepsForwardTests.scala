import org.scalatest.FlatSpec

class TwoStepsForwardTests extends FlatSpec {

  "passcode ihgpwlah" should "have a shortest path of DDRRRD" in {
    assert(TwoStepsForward.shortestPath("ihgpwlah") == "DDRRRD")
  }

  "passcode kglvqrro" should "have a shortest path of DDUDRLRRUDRD" in {
    assert(TwoStepsForward.shortestPath("kglvqrro") == "DDUDRLRRUDRD")
  }

  "passcode ulqzkmiv" should "have a shortest path of DRURDRUDDLLDLUURRDULRLDUUDDDRR" in {
    assert(TwoStepsForward.shortestPath("ulqzkmiv") == "DRURDRUDDLLDLUURRDULRLDUUDDDRR")
  }
}
