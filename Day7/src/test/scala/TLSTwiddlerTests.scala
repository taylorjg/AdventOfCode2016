import org.scalatest.FlatSpec

class TLSTwiddlerTests extends FlatSpec {

  "example 1" should "support TLS" in {
    assert(TLSTwiddler.supportsTLS("abba[mnop]qrst"))
  }

  "example 2" should "not support TLS" in {
    assert(!TLSTwiddler.supportsTLS("abcd[bddb]xyyx"))
  }

  "example 3" should "not support TLS" in {
    assert(!TLSTwiddler.supportsTLS("aaaa[qwer]tyui"))
  }

  "example 4" should "support TLS" in {
    assert(TLSTwiddler.supportsTLS("ioxxoj[asdfgh]zxcvbn"))
  }
}
