import java.io.{DataOutputStream, File, FileOutputStream, RandomAccessFile}
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import java.nio.charset.StandardCharsets

/**
 * Class to abstract SSTable read and write queries
 */
class SSTable {
  private val BlockSizeThreshold = 100 // Change later
  private type Entry = (String, String)
  private case class Index(key: String, offset: Long)
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
      val (entryKey, entryValue) = readNextEntry()
      if (entryKey == key) return Some(entryValue)
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

        val entrySize = writeEntry(file, key, value)
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
        fileOffset += writeIndex(file, index)
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
      val indexKey = readString()
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

  /**
   * Helper function to read an entry in the SSTable block(s)
   * @return Entry object parsed from file
   */
  private def readNextEntry(): Entry = {
    val entryKey = readString()
    val entryValue = readString()
    (entryKey, entryValue)
  }

  /**
   * Helper function to read a string from an SSTable file
   * @return The newly read string
   */
  private def readString(): String = {
    val len = cursor.readInt()
    val buf = new Array[Byte](len)
    cursor.readFully(buf)
    new String(buf, StandardCharsets.UTF_8)
  }

  /**
   * Helper function to write entry to file
   * @param out   Output file stream
   * @param key   Key of entry
   * @param value Value of entry
   * @return Size of entry on disk
   */
  private def writeEntry(out: DataOutputStream, key: String, value: String): Int = {
    writeString(out, key)
    writeString(out, value)
    4 + key.getBytes("UTF-8").length + 4 + value.getBytes("UTF-8").length
  }

  /**
   * Helper function to write index to file
   * @param out   Output file stream
   * @param index Index for block
   * @return Size of index on disk
   */
  private def writeIndex(out: DataOutputStream, index: Index): Int = {
    writeString(out, index.key)
    out.writeLong(index.offset)
    4 + index.key.getBytes("UTF-8").length + 4
  }

  /**
   * Helper function to write a string and its length to an SSTable file
   * @param out Output file stream
   * @param s   String to write to file stream
   */
  private def writeString(out: DataOutputStream, s: String): Unit = {
    val bytes = s.getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
  }
}

object SSTable {
  /**
   * Static function to create SSTable from a MemTable
   * @param memTable MemTable to convert to SSTable
   * @param filename Name of file to flush SStable to
   * @return SSTable object
   */
  def fromMemTable(memTable: MemTable, filename: String): SSTable = {
    val sst = new SSTable()
    sst.flush(memTable, filename).initialize()
  }

  /**
   * Static function to retrieve an SSTable from disk
   * @param filename Name of for that SSTable resides in
   * @return SSTable object
   */
  def fromDisk(filename: String): SSTable = {
    val sst = new SSTable()
    sst.cursor = new RandomAccessFile(filename, "r")
    sst.initialize()
  }
}