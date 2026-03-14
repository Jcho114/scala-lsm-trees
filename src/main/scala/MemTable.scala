import scala.annotation.static
import scala.collection.mutable

/**
 * MemTable class for in memory read and writes
 */
class MemTable {
  // Red-Black tree from stdlib
  // Plan to swap out with different custom implementations later
  private val map: mutable.TreeMap[String, String] = mutable.TreeMap()
  private var sizeBytes: Long = 0

  /**
   * Place a key-value pair to the table
   * @param key Key
   * @param value Value
   */
  def put(key: String, value: String): Unit = {
    map.get(key) match {
      case None =>
      case Some(value) => sizeBytes -= key.length + value.length + MemTable.EntryOverheadBytes
    }
    sizeBytes += key.length + value.length + MemTable.EntryOverheadBytes
    map.addOne(key, value)
  }

  /**
   * Retrieve a key-value pair from the table
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def get(key: String): Option[String] = map.get(key)

  /**
   * Delete a key-value pair from the table
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def delete(key: String): Option[String] = {
    val res = map.get(key)
    put(key, MemTable.Tombstone)
    res
  }

  /**
   * Provides size of MemTable (if in disk) in bytes
   * @return size of table in bytes
   */
  def sizeInBytes(): Long = sizeBytes
}

object MemTable {
  val EntryOverheadBytes: Int = 16 // Set to actual value later
  val Tombstone = "__TOMBSTONE__"
}