case class LiteralValue(v: Int) extends Value {
  def value(botGraph: BotGraph): Option[Int] = Some(v)
  override def toString: String = s"LiteralValue($v)"
}
