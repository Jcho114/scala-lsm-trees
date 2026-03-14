import scala.collection.mutable

/**
 * MemTable class for in memory read and writes
 */
class MemTable {
  // Red-Black tree from stdlib
  // Plan to swap out with different custom implementations later
  private val map: mutable.TreeMap[String, String] = mutable.TreeMap()
  private var sizeBytes: Long = 0

  val EntryOverheadBytes: Int = 16 // Set to actual value later

  /**
   * Place a key-value pair to the table
   * @param key Key
   * @param value Value
   */
  def put(key: String, value: String): Unit = {
    map.get(key) match {
      case None =>
      case Some(value) => sizeBytes -= key.length + value.length + EntryOverheadBytes
    }
    sizeBytes += key.length + value.length + EntryOverheadBytes
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
  def delete(key: String): Option[String] = map.remove(key) match {
    case Some(value) =>
      sizeBytes -= key.length + value.length + EntryOverheadBytes
      Some(value)
    case None => None
  }

  /**
   * Provides size of MemTable (if in disk) in bytes
   * @return size of table in bytes
   */
  def sizeInBytes(): Long = sizeBytes
}