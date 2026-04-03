import java.io.{DataOutputStream, File, FileOutputStream}

class WriteAheadLog(filename: String) {
  private val file = new DataOutputStream(new FileOutputStream(filename, true))

  /**
   * Write entry to WAL
   * @param key Key
   * @param value Value
   */
  def write(key: String, value: String): Unit = {
    writeString(key)
    writeString(value)
  }

  /**
   * Close WAL and delete it from the filesystem
   */
  def close(): Unit = {
    file.close()
    val tempFile = new File(filename)
    if (tempFile.exists()) tempFile.delete()
  }

  /**
   * Helper function to write a string and its length to a file
   * @param s String to write to file stream
   */
  private def writeString(s: String): Unit = {
    val bytes = s.getBytes("UTF-8")
    file.writeInt(bytes.length)
    file.write(bytes)
  }
}
