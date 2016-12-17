case class BotValue(fromBotNumber: Int, level: Level.Value) extends Value {
  private var cachedValue: Option[Int] = None
  def value(botGraph: BotGraph): Option[Int] =
    cachedValue match {
      case Some(_) => cachedValue
      case None =>
        cachedValue = botGraph.getBot(fromBotNumber).value(botGraph, level)
        cachedValue
    }
  override def toString: String = s"BotValue($fromBotNumber, $level)"
}
