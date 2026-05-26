import java.io.{DataOutputStream, File, FileOutputStream, RandomAccessFile}
import scala.collection.mutable.ArrayBuffer

class WriteAheadLog(filename: String) {
  val entries: ArrayBuffer[Entry] = ArrayBuffer.empty[Entry]
  initEntriesFromFile(filename)
  private val file = new DataOutputStream(new FileOutputStream(filename, true))

  private def initEntriesFromFile(filename: String): Unit = {
    val tempFile = new File(filename)
    if (!tempFile.exists()) return
    val file = new RandomAccessFile(filename, "r")
    while (file.getFilePointer < file.length()) {
      val entry = ReaderWriterUtils.readNextEntry(file)
      entries.addOne(entry)
    }
  }

  /**
   * Write entry to WAL
   * @param key Key
   * @param value Value
   */
  def write(key: String, value: String): Unit = {
    ReaderWriterUtils.writeString(file, key)
    ReaderWriterUtils.writeString(file, value)
    entries.addOne(Entry(key, value))
  }

  /**
   * Close WAL and delete it from the filesystem
   */
  def close(): Unit = {
    file.close()
    val tempFile = new File(filename)
    if (tempFile.exists()) tempFile.delete()
  }
}

object WriteAheadLog {
  val Regex = """\d{6}\.wal"""
}