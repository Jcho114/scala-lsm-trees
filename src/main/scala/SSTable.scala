import java.io.{DataOutputStream, File, FileOutputStream, RandomAccessFile}
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

/**
 * Class to abstract SSTable read and write queries
 */
class SSTable extends AutoCloseable {
  private val BlockSizeThreshold = 100 // Change later
  private var cursor: RandomAccessFile = uninitialized
  private var indexOffset: Long = uninitialized
  private var indexSize: Long = uninitialized
  private val indices = ArrayBuffer.empty[Index]

  /**
   * Function to initialize SSTable
   */
  def initialize(): this.type = {
    readIndexMetadata()
    readSparseIndex()
    this
  }

  /**
   * Static method to look up a value for a key
   * @param key Key to search for
   * @return Some value if key exists otherwise none
   */
  def findEntry(key: String): Option[String] = {
    val i = indexOfKeyInIndices(key)
    if (i == -1) return None

    val keyIndex = indices(i)
    val nextKeyOffset = if (i == indices.length - 1) indexOffset else indices(i + 1).offset
    cursor.seek(keyIndex.offset)
    while (cursor.getFilePointer < nextKeyOffset) {
      val entry = ReaderWriterUtils.readNextEntry(cursor)
      if (entry.key == key) return Some(entry.value)
    }
    None
  }

  /**
   * Method to flush MemTable in memory to SSTable on disk
   * @param memTable MemTable to flush
   * @param filename Name of file to flush to
   */
  def flush(memTable: MemTable, filename: String): this.type = {
    val file = new DataOutputStream(new FileOutputStream(new File(filename)))
    try {
      var blockStartOffset = 0
      var currentBlockSize = 0
      var firstKeyInBlock: String | Null = null
      val indices = ArrayBuffer.empty[Index]
      var fileOffset = 0

      for ((key, value) <- memTable) {
        if (firstKeyInBlock == null) {
          firstKeyInBlock = key
        }

        val entrySize = ReaderWriterUtils.writeEntry(file, key, value)
        fileOffset += entrySize
        currentBlockSize += entrySize

        if (currentBlockSize >= BlockSizeThreshold) {
          indices.addOne(Index(firstKeyInBlock, blockStartOffset))
          blockStartOffset = fileOffset
          firstKeyInBlock = null
          currentBlockSize = 0
        }
      }

      if (firstKeyInBlock != null) indices += Index(firstKeyInBlock, blockStartOffset)

      val indexOffset = fileOffset
      for (index <- indices) {
        fileOffset += ReaderWriterUtils.writeIndex(file, index)
      }

      val indexSize = fileOffset - indexOffset
      file.writeLong(indexOffset)
      file.writeLong(indexSize)
      file.flush()
    } finally file.close()
    cursor = new RandomAccessFile(filename, "r")
    this
  }

  /**
   * Helper function to read index metadata
   */
  private def readIndexMetadata(): Unit = {
    cursor.seek(cursor.length() - 16)
    indexOffset = cursor.readLong()
    indexSize = cursor.readLong()
  }

  /**
   * Helper function to retrieve sparse index from SSTable
   */
  private def readSparseIndex(): Unit = {
    cursor.seek(indexOffset)
    while (cursor.getFilePointer - indexOffset < indexSize) {
      val indexKey = ReaderWriterUtils.readString(cursor)
      val offset = cursor.readLong()
      indices += Index(indexKey, offset)
    }
  }

  /**
   * Helper function to lookup key from sparse index
   * @param key     Key to search for
   * @return Index of sparse index to start the lookup from
   */
  private def indexOfKeyInIndices(key: String): Int = {
    var (l, r) = (0, indices.length - 1)
    while (l <= r) {
      val c = (l + r) / 2
      if (key < indices(c).key) r = c - 1
      else if (key > indices(c).key) l = c + 1
      else return c
    }
    r
  }

  override def close(): Unit = {
    if (cursor != null) cursor.close()
  }
}

object SSTable {
  val Regex = """\d{6}\.sst"""

  /**
   * Static function to create SSTable from a MemTable
   * @param memTable MemTable to convert to SSTable
   * @param filename Name of file to flush SStable to
   * @return SSTable object
   */
  def fromMemTable(memTable: MemTable, filename: String): SSTable = {
    val sst = new SSTable()
    sst.flush(memTable, filename)
    try sst.initialize()
    catch {
      case e: Throwable =>
        sst.close()
        throw e
    }
  }

  /**
   * Static function to retrieve an SSTable from disk
   * @param filename Name of for that SSTable resides in
   * @return SSTable object
   */
  def fromDisk(filename: String): SSTable = {
    val sst = new SSTable()
    sst.cursor = new RandomAccessFile(filename, "r")
    try sst.initialize()
    catch {
      case e: Throwable =>
        sst.close()
        throw e
    }
  }
}