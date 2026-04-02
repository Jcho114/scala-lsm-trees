import java.io.{DataOutputStream, File, FileOutputStream}
import scala.collection.mutable.ArrayBuffer

/**
 * Class to flush MemTable in memory to SSTable on disk
 */
object SSTableWriter {
  private val BlockSizeThreshold = 100 // Change later
  private type Entry = (String, String)
  private case class Index(key: String, offset: Long)

  /**
   * Static method to flush MemTable in memory to SSTable on disk
   * @param memTable MemTable to flush
   * @param filename Name of file to flush to
   */
  def flush(memTable: MemTable, filename: String): Unit = {
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
  }

  /**
   * Helper function to write entry to file
   * @param out Output file stream
   * @param key Key of entry
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
   * @param out Output file stream
   * @param index Index for block
   * @return Size of index on disk
   */
  private def writeIndex(out: DataOutputStream, index: Index): Int = {
    writeString(out, index.key)
    out.writeLong(index.offset)
    4 + index.key.getBytes("UTF-8").length + 4
  }

  private def writeString(out: DataOutputStream, s: String): Unit = {
    val bytes = s.getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
  }
}