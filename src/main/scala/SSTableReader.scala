import java.io.RandomAccessFile
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets

/**
 * Class to lookup keys from an SSTable on disk
 */
object SSTableReader {
  private type Entry = (String, String)
  private case class Index(key: String, offset: Long)

  /**
   * Static method to look up a value for a key from an SSTable on disk
   * @param key Key to search for
   * @param filename Name of SSTable file on disk
   * @return Some value if key exists otherwise none
   */
  def findEntry(key: String, filename: String): Option[String] = {
    val file = new RandomAccessFile(filename, "r")
    try {
      val (indexOffset, indexSize) = readIndexMetadata(file)
      val indices = readSparseIndex(file, indexOffset, indexSize)
      val i = indexOfKeyInIndices(key, indices)
      if (i == -1) return None

      val keyIndex = indices(i)
      val nextKeyOffset = if (i == indices.length - 1) indexOffset else indices(i + 1).offset
      file.seek(keyIndex.offset)
      while (file.getFilePointer < nextKeyOffset) {
        val (entryKey, entryValue) = readNextEntry(file)
        if (entryKey == key) return Some(entryValue)
      }
      None
    } finally file.close()
  }

  /**
   * Helper function to read index metadata
   * @param file File object for SSTable file
   * @return Index offset in bytes and index size
   */
  private def readIndexMetadata(file: RandomAccessFile): (Long, Long) = {
    file.seek(file.length() - 16)
    val (indexOffset, indexSize) = (file.readLong(), file.readLong())
    (indexOffset, indexSize)
  }

  /**
   * Helper function to retrieve sparse index from SSTable
   * @param file File object for SSTable file
   * @param indexOffset File offset for the sparse index in bytes
   * @param indexSize Size of the sparse index in bytes
   * @return Array of key offset pairs for the sparse index
   */
  private def readSparseIndex(file: RandomAccessFile, indexOffset: Long, indexSize: Long): ArrayBuffer[Index] = {
    val indices = ArrayBuffer.empty[Index]
    file.seek(indexOffset)
    while (file.getFilePointer - indexOffset < indexSize) {
      val indexKey = readString(file)
      val offset = file.readLong()
      indices += Index(indexKey, offset)
    }
    indices
  }

  /**
   * Helper function to lookup key from sparse index
   * @param key Key to search for
   * @param indices The parsed sparse index
   * @return Index of sparse index to start the lookup from
   */
  private def indexOfKeyInIndices(key: String, indices: ArrayBuffer[Index]): Int = {
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
   * @param file File object for SSTable
   * @return Entry object parsed from file
   */
  private def readNextEntry(file: RandomAccessFile): Entry = {
    val entryKey = readString(file)
    val entryValue = readString(file)
    (entryKey, entryValue)
  }

  /**
   * Helper function to read a string from an SSTable file
   * @param file File object for SSTable
   * @return The newly read string
   */
  private def readString(file: RandomAccessFile): String = {
    val len = file.readInt()
    val buf = new Array[Byte](len)
    file.readFully(buf)
    new String(buf, StandardCharsets.UTF_8)
  }
}
