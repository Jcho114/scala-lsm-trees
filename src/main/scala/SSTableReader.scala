import java.io.RandomAccessFile
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets

object SSTableReader {
  private type Entry = (String, String)
  private case class Index(key: String, offset: Long)

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

  private def readIndexMetadata(file: RandomAccessFile): (Long, Long) = {
    file.seek(file.length() - 16)
    (file.readLong(), file.readLong())
  }

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

  private def readNextEntry(file: RandomAccessFile): Entry = {
    val entryKey = readString(file)
    val entryValue = readString(file)
    (entryKey, entryValue)
  }

  private def readString(file: RandomAccessFile): String = {
    val len = file.readInt()
    val buf = new Array[Byte](len)
    file.readFully(buf)
    new String(buf, StandardCharsets.UTF_8)
  }
}
