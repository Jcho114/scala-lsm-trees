import scala.collection.mutable

/**
 * LSM Tree main driver class
 */
class LSMTree {
  private var activeMemTable = new MemTable()
  private val flushableMemTableQueue = mutable.ArrayDeque[MemTable]()
  private val listOfSSTables = mutable.ArrayDeque[String]()

  /**
   * Background thread that flushes full MemTables residing in memory
   * to SSTables residing on disk
   */
  private val flushWorker = new Thread(() => {
    while (true) {
      val memTable = flushableMemTableQueue.synchronized {
        while(flushableMemTableQueue.isEmpty) flushableMemTableQueue.wait()
        flushableMemTableQueue.head
      }

      flushMemTableToSSTable(memTable)
      flushableMemTableQueue.synchronized {
        flushableMemTableQueue.removeHead()
      }
    }
  })
  flushWorker.start()

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
    activeMemTable = new MemTable()
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
   * Flushes MemTable residing in memory to SSTable file on disk
   */
  private def flushMemTableToSSTable(memTable: MemTable): Unit = {
    val numSSTables = listOfSSTables.length
    val filename = f"${numSSTables+1}%06d"
    SSTableWriter.flush(memTable, filename)
    listOfSSTables.prepend(filename)
  }
}

private object LSMTree {
  private val MinMemTableThresholdBytes = 1000 // Configure later
}

@main def main(): Unit = {
  val tree = new LSMTree()
  for (i <- 1 to 200) {
    tree.put(s"Key$i", s"Value$i")
  }
  Thread.sleep(2000)
  for (i <- 1 to 200) {
    val res = tree.get(s"Key$i")
    assert(res.isDefined && res.get == s"Value$i")
  }
}