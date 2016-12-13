import org.scalatest.FlatSpec

class PasswordCrackerTests extends FlatSpec {
  "abc" should "give a password of 18f47a30" in {
    assert(PasswordCracker.crackPassword("abc") == "18f47a30")
  }
}
