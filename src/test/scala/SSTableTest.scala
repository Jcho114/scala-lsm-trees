import org.scalatest._
import flatspec._
import matchers._

class SSTableTest extends AnyFlatSpec with should.Matchers {
  "A SSTable" should "lookup a SSTable file correctly when created from disk" in {
    val testFileUrl = getClass.getResource("testsstable")
    assert(testFileUrl != null, "test table file not found")
    val testFileName = testFileUrl.getPath
    val sst = SSTable.fromDisk(testFileName)

    for (i <- 1 to 20) {
      val res = sst.findEntry(s"Key$i")
      res match {
        case Some(value) => assert(value == s"Value$i")
        case None => assert(false)
      }
    }

    for (i <- 100 to 120) {
      val res = sst.findEntry(s"Key$i")
      assert(res.isEmpty)
    }
  }
}
