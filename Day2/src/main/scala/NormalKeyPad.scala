object NormalKeyPad extends KeyPad {

  override final val KeyPadUps = Map(
    1 -> 1,
    2 -> 2,
    3 -> 3,
    4 -> 1,
    5 -> 2,
    6 -> 3,
    7 -> 4,
    8 -> 5,
    9 -> 6)

  override final val KeyPadDowns = Map(
    1 -> 4,
    2 -> 5,
    3 -> 6,
    4 -> 7,
    5 -> 8,
    6 -> 9,
    7 -> 7,
    8 -> 8,
    9 -> 9)

  override final val KeyPadLefts = Map(
    1 -> 1,
    2 -> 1,
    3 -> 2,
    4 -> 4,
    5 -> 4,
    6 -> 5,
    7 -> 7,
    8 -> 7,
    9 -> 8)

  override final val KeyPadRights = Map(
    1 -> 2,
    2 -> 3,
    3 -> 3,
    4 -> 5,
    5 -> 6,
    6 -> 6,
    7 -> 8,
    8 -> 9,
    9 -> 9)
}
