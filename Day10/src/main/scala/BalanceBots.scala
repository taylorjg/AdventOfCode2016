object BalanceBots {

  def processInstructions(instructions: Seq[String]): BotGraph = {
    val (as, bs) = instructions partition (SetValueRegex.pattern.matcher(_).matches)
    val botGraph1 = as.foldLeft(new BotGraph)(processSetValueInstruction)
    val botGraph2 = bs.foldLeft(botGraph1)(processLowHighInstruction)
    botGraph2
  }

  private def processSetValueInstruction(botGraph: BotGraph, instruction: String): BotGraph = {
    val m = SetValueRegex.findAllIn(instruction)
    val value = m.group(1).toInt
    val botNumber = m.group(2).toInt
    botGraph.addValueToBot(botNumber, value)
  }

  private def processLowHighInstruction(botGraph: BotGraph, instruction: String): BotGraph = {
    val m = LowHighRegex.findAllIn(instruction)
    val botNumber = m.group(1).toInt
    val lowTo = m.group(2)
    val highTo = m.group(3)
    val lowBotNumber = if (lowTo.startsWith("bot")) Some(extractNumberFrom(lowTo)) else None
    val lowOutputNumber = if (lowTo.startsWith("output")) Some(extractNumberFrom(lowTo)) else None
    val highBotNumber = if (highTo.startsWith("bot")) Some(extractNumberFrom(highTo)) else None
    val highOutputNumber = if (highTo.startsWith("output")) Some(extractNumberFrom(highTo)) else None

    val botGraph1 = (lowBotNumber, lowOutputNumber) match {
      case (Some(lbn), None) => botGraph.connectBots(botNumber, lbn, isLow = true)
      case (None, Some(lon)) => botGraph.addOutput(botNumber, lon, isLow = true)
    }

    val botGraph2 = (highBotNumber, highOutputNumber) match {
      case (Some(hbn), None) => botGraph1.connectBots(botNumber, hbn, isLow = false)
      case (None, Some(hon)) => botGraph1.addOutput(botNumber, hon, isLow = false)
    }

    botGraph2
  }

  private def extractNumberFrom(s: String): Int = NumberRegex.findAllIn(s).group(1).toInt

  private final val SetValueRegex = """value (\d+) goes to bot (\d+)""".r
  private final val LowHighRegex = """bot (\d+) gives low to (.+) and high to (.+)""".r
  private final val NumberRegex = """.+ (\d+)""".r
}
