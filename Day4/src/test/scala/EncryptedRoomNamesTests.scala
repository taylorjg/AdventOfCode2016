import org.scalatest.FlatSpec

class EncryptedRoomNamesTests extends FlatSpec {

  "example 1" should "be a valid room" in {
    assert(EncryptedRoomNames.validateRoom("aaaaa-bbb-z-y-x-123[abxyz]").contains(("aaaaa-bbb-z-y-x", 123)))
  }

  "example 2" should "be a valid room" in {
    assert(EncryptedRoomNames.validateRoom("a-b-c-d-e-f-g-h-987[abcde]").contains(("a-b-c-d-e-f-g-h", 987)))
  }

  "example 3" should "be a valid room" in {
    assert(EncryptedRoomNames.validateRoom("not-a-real-room-404[oarel]").contains(("not-a-real-room", 404)))
  }

  "example 4" should "be an invalid room" in {
    assert(EncryptedRoomNames.validateRoom("totally-real-room-200[decoy]").isEmpty)
  }

  "example room name" should "should be decrypted correctly" in {
    assert(EncryptedRoomNames.decryptRoomName("qzmt-zixmtkozy-ivhz", 343) == "very encrypted name")
  }
}
