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
    memTable.delete("Key1")
    memTable.get("Key1") should be (None)
  }
}