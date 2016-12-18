class Registers(val map: Map[String, Register]) {
  def this() = this(Map(
    "a" -> Register("a", 0),
    "b" -> Register("b", 0),
    "c" -> Register("c", 0),
    "d" -> Register("d", 0)))
}
