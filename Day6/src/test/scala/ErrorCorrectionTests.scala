import org.scalatest.FlatSpec

class ErrorCorrectionTests extends FlatSpec {

  "given example" should "return easter" in {
    val input =
      """
        |eedadn
        |drvtee
        |eandsr
        |raavrd
        |atevrs
        |tsrnev
        |sdttsa
        |rasrtv
        |nssdts
        |ntnada
        |svetve
        |tesnvt
        |vntsnd
        |vrdear
        |dvrsen
        |enarar
      """.stripMargin
    val lines = input.split("\n").toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    assert(ErrorCorrection.correct(cleanedLines) == "easter")
  }
}
