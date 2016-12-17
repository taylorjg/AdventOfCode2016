import Level._

object BalanceBots {

  def processInstructions(instructions: Seq[String]): BotGraph = {
    val (xs, ys) = instructions partition (SetValueRegex.pattern.matcher(_).matches)
    val botGraph1 = xs.foldLeft(new BotGraph)(processSetValueInstruction)
    val botGraph2 = ys.foldLeft(botGraph1)(processConnectBotsInstruction)
    botGraph2
  }

  private def processSetValueInstruction(botGraph: BotGraph, instruction: String): BotGraph = {
    val m = SetValueRegex.findAllIn(instruction)
    val value = m.group(1).toInt
    val botNumber = m.group(2).toInt
    botGraph.setValue(botNumber, value)
  }

  private def processConnectBotsInstruction(botGraph: BotGraph, instruction: String): BotGraph = {
    val m = ConnectBotsRegex.findAllIn(instruction)
    val fromBotNumber = m.group(1).toInt
    val lowTo = m.group(2)
    val highTo = m.group(3)
    val lowBotNumber = if (lowTo.startsWith("bot")) Some(extractNumberFrom(lowTo)) else None
    val lowOutputNumber = if (lowTo.startsWith("output")) Some(extractNumberFrom(lowTo)) else None
    val highBotNumber = if (highTo.startsWith("bot")) Some(extractNumberFrom(highTo)) else None
    val highOutputNumber = if (highTo.startsWith("output")) Some(extractNumberFrom(highTo)) else None

    val botGraph1 = (lowBotNumber, lowOutputNumber) match {
      case (Some(lbn), None) => botGraph.connectBots(fromBotNumber, lbn, Low)
      case (None, Some(lon)) => botGraph.connectOutput(fromBotNumber, lon, Low)
      case _ => throw new Exception(s"""Bot $fromBotNumber is missing it's low destination""")
    }

    (highBotNumber, highOutputNumber) match {
      case (Some(hbn), None) => botGraph1.connectBots(fromBotNumber, hbn, High)
      case (None, Some(hon)) => botGraph1.connectOutput(fromBotNumber, hon, High)
      case _ => throw new Exception(s"""Bot $fromBotNumber is missing it's high destination""")
    }
  }

  private def extractNumberFrom(s: String): Int = NumberRegex.findAllIn(s).group(1).toInt

  private final val SetValueRegex = """value (\d+) goes to bot (\d+)""".r
  private final val ConnectBotsRegex = """bot (\d+) gives low to (.+) and high to (.+)""".r
  private final val NumberRegex = """.+ (\d+)""".r
}
