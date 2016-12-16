class Bot(botNumber: Int, values: Seq[Int]) {
  val complete: Boolean = values.length == 2
  val low: Option[Int] = if (complete) Some(values.min) else None
  val high: Option[Int] = if (complete) Some(values.max) else None
  def addValue(value: Int): Bot = {
    if (complete) throw new Exception(s"Attempt to add value $value to completed bot $botNumber")
    else new Bot(botNumber, value +: values)
  }
}
