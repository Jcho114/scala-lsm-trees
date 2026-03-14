import org.scalatest._
import flatspec._
import matchers._

/**
 * Tests for MemTable class
 */
class MemTableTest extends AnyFlatSpec with should.Matchers {
  "A MemTable" should "retrieve a stored key value pair" in {
    val memTable = new MemTable()
    memTable.put("Key1", "Value1")
    memTable.get("Key1") should be (Some("Value1"))
  }

  it should "update a key value pair" in {
    val memTable = new MemTable()
    memTable.put("Key1", "Value1")
    memTable.get("Key1") should be (Some("Value1"))
    memTable.put("Key1", "Value2")
    memTable.get("Key1") should be (Some("Value2"))
  }

  it should "return None for a nonexistent key" in {
    val memTable = new MemTable()
    memTable.get("Nonexistent") should be (None)
  }

  it should "properly remove a deleted element" in {
    val memTable = new MemTable()
    memTable.put("Key1", "Value1")
    memTable.get("Key1") should be (Some("Value1"))
    memTable.delete("Key1") should be (Some("Value1"))
    memTable.get("Key1") should be (None)
    memTable.delete("Key1") should be (None)
  }

  it should "keep track of its size in puts and deletes" in {
    val memTable = new MemTable()
    val (key, value) = ("Value1", "Key1")
    val bytes: Long = key.length + value.length + MemTable.EntryOverheadBytes
    memTable.put(key, value)
    memTable.sizeInBytes() should be (bytes)
    val longValue = "LongerValue"
    memTable.put(key, longValue)
    memTable.sizeInBytes() should be (bytes - value.length + longValue.length)
    memTable.delete(key)
    memTable.sizeInBytes() should be (key.length + MemTable.Tombstone.length + MemTable.EntryOverheadBytes)
  }
}