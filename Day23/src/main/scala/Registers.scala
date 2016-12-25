class Registers(val map: Map[String, Register]) {

  def this() = this(Map(
    "a" -> Register("a", 0),
    "b" -> Register("b", 0),
    "c" -> Register("c", 0),
    "d" -> Register("d", 0)))

  def this(rs: Seq[Register]) = this(rs.map(r => (r.name, r)).toMap)

  def getValue(r: String): Int = map(r).value

  def setValue(r: String, value: Int): Registers =
    new Registers(map.updated(r, Register(r, value)))

  def a: Int = getValue("a")
  def b: Int = getValue("b")
  def c: Int = getValue("c")
  def d: Int = getValue("d")
}
