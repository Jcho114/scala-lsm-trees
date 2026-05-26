import scala.collection.mutable

/**
 * MemTable class for in memory read and writes
 */
class MemTable(val id: Int) extends Iterable[(String, String)], AutoCloseable {
  // Red-Black tree from stdlib
  // Plan to swap out with different custom implementations later
  private val map: mutable.TreeMap[String, String] = mutable.TreeMap()
  private var estimatedSizeBytes: Long = 0
  var wal: Option[WriteAheadLog] = None

  def this(id: Int, wal: WriteAheadLog) = {
    this(id)
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
    wal.foreach(_.write(key, value))
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

  override def close(): Unit = {
    wal.foreach(_.close())
  }
}

object MemTable {
  val EntryOverheadBytes: Int = 8 // Set to actual value later
  val Tombstone = "__TOMBSTONE__"
}