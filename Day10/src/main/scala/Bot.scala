import Level._

class Bot(botNumber: Int, values: Seq[Value]) {
  def this(botNumber: Int) = this(botNumber, Seq())
  def low(botGraph: BotGraph): Option[Int] = evaluate(botGraph, Low)
  def high(botGraph: BotGraph): Option[Int] = evaluate(botGraph, High)
  def value(botGraph: BotGraph, level: Level.Value): Option[Int] =
    level match {
      case Low => low(botGraph)
      case High => high(botGraph)
    }
  def setValue(value: Int): Bot =
    new Bot(botNumber, LiteralValue(value) +: values)
  def connectTo(fromBotNumber: Int, level: Level.Value): Bot =
    new Bot(botNumber, BotValue(fromBotNumber, level) +: values)
  private def evaluate(botGraph: BotGraph, level: Level.Value): Option[Int] = {
    val vs = values flatMap (_.value(botGraph))
    if (vs.size == 2) {
      level match {
        case Low => Some(vs.min)
        case High => Some(vs.max)
      }
    }
    else None
  }
}
