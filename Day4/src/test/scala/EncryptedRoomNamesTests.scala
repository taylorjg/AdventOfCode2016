import org.scalatest.FlatSpec

class EncryptedRoomNamesTests extends FlatSpec {

  "example 1" should "be a valid room" in {
    assert(EncryptedRoomNames.sectorIdOfRoom("aaaaa-bbb-z-y-x-123[abxyz]") == Some(123))
  }

  "example 2" should "be a valid room" in {
    assert(EncryptedRoomNames.sectorIdOfRoom("a-b-c-d-e-f-g-h-987[abcde]") == Some(987))
  }

  "example 3" should "be a valid room" in {
    assert(EncryptedRoomNames.sectorIdOfRoom("not-a-real-room-404[oarel]") == Some(404))
  }

  "example 4" should "be an invalid room" in {
    assert(EncryptedRoomNames.sectorIdOfRoom("totally-real-room-200[decoy]") == None)
  }
}
