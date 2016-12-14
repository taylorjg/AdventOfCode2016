import org.scalatest.FlatSpec

class ErrorCorrectionTests extends FlatSpec {

  private final val Input =
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

  "given example" should "return easter" in {
    val lines = Input.split("\n").toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    assert(ErrorCorrection.correct(cleanedLines, useMostCommon = true) == "easter")
  }

  "given example" should "return advent when using other sort order" in {
    val lines = Input.split("\n").toList
    val cleanedLines = lines map (_.trim) filter (_.nonEmpty)
    assert(ErrorCorrection.correct(cleanedLines, useMostCommon = false) == "advent")
  }
}
