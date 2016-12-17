case class LiteralValue(literalValue: Int) extends Value {
  def value(botGraph: BotGraph): Option[Int] = Some(literalValue)
  override def toString: String = s"LiteralValue($literalValue)"
}
