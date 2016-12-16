import Level._

trait Value {
  def value: Option[Int]
}

case class LiteralValue(v: Int) extends Value {
  val value: Option[Int] = Some(v)
  override def toString: String = s"LiteralValue($v)"
}

case class BotValue(fromBot: Bot, level: Level.Value) extends Value {
  def value: Option[Int] = fromBot.value(level)
  override def toString: String = s"BotValue(${fromBot.botNumber}, $level)"
}

class Bot(val botNumber: Int, val values: Seq[Value]) {
  def this(botNumber: Int) = this(botNumber, Seq())
  def low: Option[Int] = evaluate(Low)
  def high: Option[Int] = evaluate(High)
  def value(level: Level.Value): Option[Int] =
    level match {
      case Low => low
      case High => high
    }
  def addValue(fromBot: Bot, level: Level.Value): Bot = {
    if (values.size == 2) throw new Exception(s"Attempt to add $level value to completed bot $botNumber")
    else new Bot(botNumber, new BotValue(fromBot, level) +: values)
  }
  def addValue(value: Int): Bot = {
    new Bot(botNumber, new LiteralValue(value) +: values)
  }
  def childBots: Seq[Bot] = values collect {
    case BotValue(b, l) => b
  }
  private def evaluate(level: Level.Value): Option[Int] = {
    val vs = values flatMap (_.value)
    if (vs.size == 2) {
      level match {
        case Low => Some(vs.min)
        case High => Some(vs.max)
      }
    }
    else None
  }
}
