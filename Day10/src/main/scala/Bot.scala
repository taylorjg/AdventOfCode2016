import Level._

trait Value {
  def value(botGraph: BotGraph): Option[Int]
}

case class LiteralValue(v: Int) extends Value {
  def value(botGraph: BotGraph): Option[Int] = Some(v)
  override def toString: String = s"LiteralValue($v)"
}

case class BotValue(fromBotNumber: Int, level: Level.Value) extends Value {
  def value(botGraph: BotGraph): Option[Int] = botGraph.getBot(fromBotNumber).value(botGraph, level)
  override def toString: String = s"BotValue($fromBotNumber, $level)"
}

class Bot(val botNumber: Int, val values: Seq[Value]) {
  def this(botNumber: Int) = this(botNumber, Seq())
  def low(botGraph: BotGraph): Option[Int] = evaluate(botGraph, Low)
  def high(botGraph: BotGraph): Option[Int] = evaluate(botGraph, High)
  def value(botGraph: BotGraph, level: Level.Value): Option[Int] =
    level match {
      case Low => low(botGraph)
      case High => high(botGraph)
    }
  def addValue(fromBotNumber: Int, level: Level.Value): Bot = {
    if (values.size == 2) throw new Exception(s"Attempt to add $level value to completed bot $botNumber")
    else new Bot(botNumber, BotValue(fromBotNumber, level) +: values)
  }
  def addValue(value: Int): Bot = {
    new Bot(botNumber, LiteralValue(value) +: values)
  }
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
