import scala.collection.mutable

/**
 * MemTable class for in memory read and writes
 */
class MemTable {
  /**
   * Red-Black tree from stdlib
   * Plan to swap out with different custom implementations later
   */
  private val map: mutable.TreeMap[String, String] = mutable.TreeMap()

  /**
   * Place a key-value pair to the table
   * @param key Key
   * @param value Value
   */
  def put(key: String, value: String): Unit = map.addOne(key, value)

  /**
   * Retrieve a key-value pair from the table
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def get(key: String): Option[String] = map.get(key)
}