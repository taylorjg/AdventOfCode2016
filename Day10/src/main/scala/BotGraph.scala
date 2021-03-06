class BotGraph(botMap: Map[Int, Bot], outputMap: Map[Int, BotValue]) {

  def this() = this(Map(), Map())

  def getBot(botNumber: Int): Bot = botMap.getOrElse(botNumber, new Bot(botNumber))

  def setValue(botNumber: Int, value: Int): BotGraph = {
    val bot = getBot(botNumber).setValue(value)
    new BotGraph(botMap.updated(botNumber, bot), outputMap)
  }

  def connectBots(fromBotNumber: Int, toBotNumber: Int, level: Level.Value): BotGraph = {
    val toBot = getBot(toBotNumber).connectTo(fromBotNumber, level)
    val newBotMap = botMap.updated(toBotNumber, toBot)
    new BotGraph(newBotMap, outputMap)
  }

  def connectOutput(fromBotNumber: Int, outputNumber: Int, level: Level.Value): BotGraph = {
    val newOutputMap = outputMap.updated(outputNumber, BotValue(fromBotNumber, level))
    new BotGraph(botMap, newOutputMap)
  }

  def findComparerOf(value1: Int, value2: Int): Option[Int] = {
    val high = Some(Math.max(value1, value2))
    val low = Some(Math.min(value1, value2))
    botMap collectFirst {
      case (bn, b) if b.high(this) == high && b.low(this) == low => bn
    }
  }

  def getOutputValue(outputNumber: Int): Option[Int] = outputMap.get(outputNumber) flatMap (_.value(this))
}
