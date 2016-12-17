class BotGraph(val botMap: Map[Int, Bot], outputMap: Map[Int, (Bot, Level.Value)]) {

  def this() = this(Map(), Map())

  def addValueToBot(botNumber: Int, value: Int): BotGraph = {
    val bot = botMap.getOrElse(botNumber, new Bot(botNumber))
    new BotGraph(botMap.updated(botNumber, bot.addValue(value)), outputMap)
  }

  def connectBots(fromBotNumber: Int, toBotNumber: Int, level: Level.Value): BotGraph = {
    val fromBot = botMap.getOrElse(fromBotNumber, new Bot(fromBotNumber))
    val toBot = botMap.getOrElse(toBotNumber, new Bot(toBotNumber))
    val toBotConnected = toBot.addValue(fromBot, level)
    val newBotMap = botMap
      .updated(fromBotNumber, fromBot)
      .updated(toBotNumber, toBotConnected)
    new BotGraph(newBotMap, outputMap)
  }

  def connectOutput(fromBotNumber: Int, outputNumber: Int, level: Level.Value): BotGraph = {
    val fromBot = botMap.getOrElse(fromBotNumber, new Bot(fromBotNumber))
    val newBotMap = botMap.updated(fromBotNumber, fromBot)
    val newOutputMap = outputMap.updated(outputNumber, (fromBot, level))
    new BotGraph(newBotMap, newOutputMap)
  }

  def findComparerOf(value1: Int, value2: Int): Option[Int] = {
    val high = Some(Math.max(value1, value2))
    val low = Some(Math.min(value1, value2))
    @annotation.tailrec
    def loop(bots: Seq[Bot]): Option[Int] = {
      bots find (b => b.low == low && b.high == high) match {
        case Some(bot) => Some(bot.botNumber)
        case None =>
          val childBots = bots flatMap (_.childBots)
          if (childBots.isEmpty) None else loop(childBots)
      }
    }
    val rootBots = botMap collect { case(_, b) if b.low.nonEmpty && b.high.nonEmpty => b }
//    println(s"--------------------------------------------------------------------------------")
//    rootBots foreach (BalanceBots.dumpBot(_, ""))
    loop(rootBots.toSeq)
  }
}
