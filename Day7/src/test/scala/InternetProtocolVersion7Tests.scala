import org.scalatest.FlatSpec

class InternetProtocolVersion7Tests extends FlatSpec {

  "TLS example 1" should "support TLS" in {
    assert(InternetProtocolVersion7.supportsTLS("abba[mnop]qrst"))
  }

  "TLS example 2" should "not support TLS" in {
    assert(!InternetProtocolVersion7.supportsTLS("abcd[bddb]xyyx"))
  }

  "TLS example 3" should "not support TLS" in {
    assert(!InternetProtocolVersion7.supportsTLS("aaaa[qwer]tyui"))
  }

  "TLS example 4" should "support TLS" in {
    assert(InternetProtocolVersion7.supportsTLS("ioxxoj[asdfgh]zxcvbn"))
  }

  "SSL example 1" should "support SSL" in {
    assert(InternetProtocolVersion7.supportsSSL("aba[bab]xyz"))
  }

  "SSL example 2" should "not support SSL" in {
    assert(InternetProtocolVersion7.supportsSSL("xyx[xyx]xyx"))
  }

  "SSL example 3" should "support SSL" in {
    assert(InternetProtocolVersion7.supportsSSL("aaa[kek]eke"))
  }

  "SSL example 4" should "support SSL" in {
    assert(InternetProtocolVersion7.supportsSSL("zazbz[bzb]cdb"))
  }
}
