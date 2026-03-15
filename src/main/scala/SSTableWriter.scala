import java.io.{DataOutputStream, File, FileOutputStream}
import scala.collection.mutable.ArrayBuffer

/**
 * Class to flush MemTable in memory to SSTable on disk
 */
object SSTableWriter {
  private val BlockSizeThreshold = 100 // Change later
  private type Entry = (String, String)
  private class Index(val key: String, val offset: Int) {}

  /**
   * Static method to flush MemTable in memory to SSTable on disk
   * @param memTable MemTable to flush
   * @param filename Name of file to flush to
   */
  def flush(memTable: MemTable, filename: String): Unit = {
    val dataOutputStream = new DataOutputStream(new FileOutputStream(new File(filename)))
    var blockStartOffset = 0
    var currentBlockSize = 0
    var firstKeyInBlock: String | Null = null
    val indices = new ArrayBuffer[Index]()
    var fileOffset = 0

    // Entry blocks
    for ((key, value) <- memTable) {
      if (firstKeyInBlock == null) {
        firstKeyInBlock = key
      }

      val entrySize = writeEntry(dataOutputStream, key, value)
      fileOffset += entrySize
      currentBlockSize += entrySize

      if (currentBlockSize >= BlockSizeThreshold) {
        indices.addOne(new Index(firstKeyInBlock, blockStartOffset))
        blockStartOffset = fileOffset
        firstKeyInBlock = null
        currentBlockSize = 0
      }
    }

    if (firstKeyInBlock != null) {
      indices.addOne(new Index(firstKeyInBlock, blockStartOffset))
    }

    val indexOffset = fileOffset

    // Sparse index
    for (index <- indices) {
      fileOffset += writeIndex(dataOutputStream, index)
    }

    // Footer
    val indexSize = fileOffset - indexOffset
    dataOutputStream.writeLong(indexOffset)
    dataOutputStream.writeLong(indexSize)

    dataOutputStream.flush()
    dataOutputStream.close()
  }

  /**
   * Helper function to write entry to file
   * @param dataOutputStream Output file stream
   * @param key Key of entry
   * @param value Value of entry
   * @return Size of entry on disk
   */
  private def writeEntry(dataOutputStream: DataOutputStream, key: String, value: String): Int = {
    val kb = key.getBytes("UTF-8")
    val vb = value.getBytes("UTF-8")
    dataOutputStream.writeInt(kb.length)
    dataOutputStream.write(kb)
    dataOutputStream.writeInt(vb.length)
    dataOutputStream.write(vb)
    4 + kb.length + 4 + vb.length
  }

  /**
   * Helper function to write index to file
   * @param dataOutputStream Output file stream
   * @param index Index for block
   * @return Size of index on disk
   */
  private def writeIndex(dataOutputStream: DataOutputStream, index: Index): Int = {
    val kb = index.key.getBytes("UTF-8")
    dataOutputStream.writeInt(kb.length)
    dataOutputStream.write(kb)
    dataOutputStream.writeInt(index.offset)
    4 + kb.length + 4
  }
}