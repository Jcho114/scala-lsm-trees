import org.scalatest.*
import flatspec.*
import matchers.*

import java.io.File

class SSTableTest extends AnyFlatSpec with should.Matchers {
  "A SSTable" should "lookup a SSTable file correctly when created from disk" in {
    val testFileUrl = getClass.getResource("testsstable")
    assert(testFileUrl != null, "test table file not found")
    val testFileName = testFileUrl.getPath
    val sst = SSTable.fromDisk(testFileName)

    for (i <- 1 to 20) {
      val res = sst.findEntry(f"Key$i")
      res match {
        case Some(value) => assert(value == f"Value$i")
        case None => assert(false)
      }
    }

    for (i <- 100 to 120) {
      val res = sst.findEntry(f"Key$i")
      assert(res.isEmpty)
    }
  }

  it should "lookup a SSTable file correctly when created from a MemTable" in {
    val filename = "sstabletempfile"
    val memTable = MemTable()
    for (i <- 1 to 10) {
      memTable.put(f"Key$i", f"Value$i")
    }
    val sst = SSTable.fromMemTable(memTable, filename)

    for (i <- 1 to 10) {
      val res = sst.findEntry(f"Key$i")
      res match {
        case Some(value) => assert(value == f"Value$i")
        case None => assert(false)
      }
    }

    for (i <- 100 to 120) {
      val res = sst.findEntry(f"Key$i")
      assert(res.isEmpty)
    }

    val tempFile = new File(filename)
    if (tempFile.exists()) tempFile.delete()
  }
}
