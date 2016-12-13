import org.scalatest.FlatSpec

class PasswordCrackerTests extends FlatSpec {

  "abc" should "give a part 1 password of 18f47a30" in {
    assert(PasswordCracker.crackPassword("abc") == "18f47a30")
  }

  "abc" should "give a part 2 password of 05ace8e3" in {
    assert(PasswordCracker.crackPassword2("abc") == "05ace8e3")
  }
}
