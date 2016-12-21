object Main {
  def main(args: Array[String]): Unit = {

    val answer1 = TwoStepsForward.shortestPath("vwbaicqe")
    println(s"answer1: $answer1")

    val answer2 = TwoStepsForward.longestPathNumSteps("vwbaicqe")
    println(s"answer2: $answer2")
  }
}
