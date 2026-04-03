import java.io.{DataOutputStream, RandomAccessFile}
import java.nio.charset.StandardCharsets

object ReaderWriterUtils {
  /**
   * Helper function to read an entry from a file
   * @return Entry object parsed from file
   */
  def readNextEntry(cursor: RandomAccessFile): Entry = {
    val entryKey = readString(cursor)
    val entryValue = readString(cursor)
    Entry(entryKey, entryValue)
  }

  /**
   * Helper function to read a string from a file
   * @return The newly read string
   */
  def readString(cursor: RandomAccessFile): String = {
    val len = cursor.readInt()
    val buf = new Array[Byte](len)
    cursor.readFully(buf)
    new String(buf, StandardCharsets.UTF_8)
  }

  /**
   * Helper function to write an entry to a file
   * @param out   Output file stream
   * @param key   Key of entry
   * @param value Value of entry
   * @return Size of entry on disk
   */
  def writeEntry(out: DataOutputStream, key: String, value: String): Int = {
    writeString(out, key)
    writeString(out, value)
    4 + key.getBytes("UTF-8").length + 4 + value.getBytes("UTF-8").length
  }

  /**
   * Helper function to write an index to a file
   * @param out   Output file stream
   * @param index Index for block
   * @return Size of index on disk
   */
  def writeIndex(out: DataOutputStream, index: Index): Int = {
    writeString(out, index.key)
    out.writeLong(index.offset)
    4 + index.key.getBytes("UTF-8").length + 4
  }

  /**
   * Helper function to write a string and its length to a file
   * @param out Output file stream
   * @param s   String to write to file stream
   */
  def writeString(out: DataOutputStream, s: String): Unit = {
    val bytes = s.getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
  }
}
