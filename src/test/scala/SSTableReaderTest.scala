import org.scalatest._
import flatspec._
import matchers._

class SSTableReaderTest extends AnyFlatSpec with should.Matchers {
  "A SSTableReader" should "lookup a SSTable file correctly" in {
    val testFileUrl = getClass.getResource("testsstable")
    assert(testFileUrl != null, "test table file not found")
    val testFileName = testFileUrl.getPath

    for (i <- 1 to 20) {
      val res = SSTableReader.findEntry(s"Key$i", testFileName)
      res match {
        case Some(value) => assert(value == s"Value$i")
        case None => assert(false)
      }
    }

    for (i <- 100 to 120) {
      val res = SSTableReader.findEntry(s"Key$i", testFileName)
      assert(res.isEmpty)
    }
  }
}
