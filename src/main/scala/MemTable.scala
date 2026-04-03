import scala.collection.mutable
import scala.language.postfixOps

/**
 * MemTable class for in memory read and writes
 */
class MemTable extends Iterable[(String, String)] {
  // Red-Black tree from stdlib
  // Plan to swap out with different custom implementations later
  private val map: mutable.TreeMap[String, String] = mutable.TreeMap()
  private var estimatedSizeBytes: Long = 0
  var wal: Option[WriteAheadLog] = None
  val id: Int = MemTable.counter
  MemTable.counter = MemTable.counter+1

  def this(wal: WriteAheadLog) = {
    this()
    for (entry <- wal.entries) {
      put(entry.key, entry.value)
    }
    this.wal = Some(wal)
  }

  override def iterator: Iterator[(String, String)] = map.iterator

  /**
   * Place a key-value pair to the table
   * @param key Key
   * @param value Value
   */
  def put(key: String, value: String): Unit = {
    wal.foreach(wal => wal.write(key, value))
    map.get(key) match {
      case None =>
      case Some(value) => estimatedSizeBytes -= key.getBytes.length + value.getBytes.length + MemTable.EntryOverheadBytes
    }
    estimatedSizeBytes += key.getBytes.length + value.getBytes.length + MemTable.EntryOverheadBytes
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
    val res = get(key)
    put(key, MemTable.Tombstone)
    res
  }

  /**
   * Provides size of MemTable (if in disk) in bytes
   * @return size of table in bytes
   */
  def estimatedSizeInBytes(): Long = estimatedSizeBytes
}

object MemTable {
  val EntryOverheadBytes: Int = 8 // Set to actual value later
  val Tombstone = "__TOMBSTONE__"
  var counter = 0
}