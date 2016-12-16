import Level._

class BotGraph(botMap: Map[Int, Bot], outputMap: Map[Int, (Bot, Level.Value)]) {

  def this() = this(Map(), Map())

  def addValueToBot(botNumber: Int, value: Int): BotGraph =
    botMap get botNumber match {
      case Some(bot) => new BotGraph(botMap.updated(botNumber, bot.addValue(value)), outputMap)
      case None => new BotGraph(botMap + (botNumber -> new Bot(botNumber, Seq(value))), outputMap)
    }

  def connectBots(fromBotNumber: Int, toBotNumber: Int, level: Level.Value): BotGraph = {
    val fromBot = botMap.getOrElse(fromBotNumber, new Bot(fromBotNumber, Seq()))
    val toBot = botMap.getOrElse(toBotNumber, new Bot(toBotNumber, Seq()))
    val toBot2 = if (fromBot.complete) {
      val value = if (level == Low) fromBot.low.get else fromBot.high.get
      toBot.addValue(value)
    } else toBot
    new BotGraph(botMap.updated(fromBotNumber, fromBot).updated(toBotNumber, toBot2), outputMap)
  }

  def addOutput(botNumber: Int, outputNumber: Int, level: Level.Value): BotGraph =
    botMap get botNumber match {
      case Some(bot) => new BotGraph(botMap, outputMap + (outputNumber -> (bot, level)))
      case None =>
        val bot = new Bot(botNumber, Seq())
        new BotGraph(botMap + (botNumber -> bot), outputMap + (outputNumber -> (bot, level)))
    }

  def findComparerOf(value1: Int, value2: Int): Option[Int] = {
    val high = Some(Math.max(value1, value2))
    val low = Some(Math.min(value1, value2))
    botMap collectFirst { case (bn, b) if b.high == high && b.low == low => bn }
  }
}
