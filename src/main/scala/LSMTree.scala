import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * LSM Tree main driver class
 */
class LSMTree {
  private var activeMemTable: MemTable = uninitialized
  private val flushableMemTableQueue = mutable.ArrayDeque[MemTable]()
  private val listOfSSTables = mutable.ArrayDeque[String]()
  private val flushWorkerIsRunning = new AtomicBoolean(true)
  private var filePath = ""

  /**
   * Background thread that flushes full MemTables residing in memory
   * to SSTables residing on disk
   */
  private val flushWorker = new Thread(() => {
    while (flushWorkerIsRunning.get()) {
      val memTableOpt = flushableMemTableQueue.synchronized {
        while(flushableMemTableQueue.isEmpty && flushWorkerIsRunning.get()) flushableMemTableQueue.wait()
        flushableMemTableQueue.headOption
      }
      memTableOpt match {
        case Some(memTable) => flushMemTableToSSTable(memTable)
          flushableMemTableQueue.synchronized {
            flushableMemTableQueue.removeHead()
          }
        case None =>
      }
    }
  })

  /**
   * Function to open the lsm tree
   * @param path Path to lsm tree
   */
  def open(path: String): Unit = {
    filePath = path
    val dir = new File(filePath)
    if (!dir.exists()) dir.mkdirs()
    createNewActiveMemTable()
    flushWorker.start()
  }

  /**
   * Put key-value pair to lsm-tree
   * @param key Key
   * @param value Value
   * @return True if put succeeds otherwise False
   */
  def put(key: String, value: String): Boolean = {
    if (value == MemTable.Tombstone) return false // Do not allow user to delete with tombstone
    activeMemTable.put(key, value)
    if (activeMemTable.estimatedSizeInBytes() >= LSMTree.MinMemTableThresholdBytes) {
      makeActiveMemTableFlushable()
    }
    true
  }

  /**
   * Helper function to append active MemTable to flush queue
   */
  private def makeActiveMemTableFlushable(): Unit = {
    flushableMemTableQueue.synchronized {
      flushableMemTableQueue.append(activeMemTable)
      flushableMemTableQueue.notify()
    }
    createNewActiveMemTable()
  }

  private def createNewActiveMemTable(): Unit = {
    val wal = new WriteAheadLog(f"$filePath/${MemTable.counter}%06d.wal")
    activeMemTable = new MemTable(wal)
  }

  /**
   * Get corresponding value from lsm-tree if key exists
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def get(key: String): Option[String] = {
    var tables: Iterator[MemTable] = Iterator()
    flushableMemTableQueue.synchronized {
      tables = Iterator.single(activeMemTable) ++ flushableMemTableQueue.reverseIterator
    }

    for (table <- tables.iterator) {
      table.get(key) match {
        case Some(MemTable.Tombstone) => return None
        case Some(v) => return Some(v)
        case None =>
      }
    }

    for (filename <- listOfSSTables) {
      SSTableReader.findEntry(key, filename) match {
        case Some(MemTable.Tombstone) => return None
        case Some(v) => return Some(v)
        case None =>
      }
    }

    None
  }

  /**
   * Delete key-value pair from lsm-tree
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def delete(key: String): Option[String] = activeMemTable.delete(key)

  /**
   * Function to close the lsm-tree process
   */
  def close(): Unit = {
    flushWorkerIsRunning.set(false)
    flushableMemTableQueue.synchronized {
      flushableMemTableQueue.notify()
    }
    flushWorker.join()
  }

  /**
   * Flushes MemTable residing in memory to SSTable file on disk
   */
  private def flushMemTableToSSTable(memTable: MemTable): Unit = {
    val filename = f"$filePath/${memTable.id}%06d"
    SSTableWriter.flush(memTable, filename)
    memTable.wal.foreach(wal => wal.close())
    listOfSSTables.prepend(filename)
  }
}

private object LSMTree {
  private val MinMemTableThresholdBytes = 1000 // Configure later
}

@main def main(): Unit = {
  val tree = new LSMTree()
  tree.open("testdb")
  for (i <- 1 to 200) {
    tree.put(s"Key$i", s"Value$i")
  }
  Thread.sleep(2000)
  for (i <- 1 to 200) {
    val res = tree.get(s"Key$i")
    assert(res.isDefined && res.get == s"Value$i")
  }
  tree.close()
}