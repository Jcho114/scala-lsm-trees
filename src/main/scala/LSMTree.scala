import scala.collection.mutable

/**
 * LSM Tree main driver class
 */
class LSMTree {
  private var activeMemTable = new MemTable()
  private val flushableMemTableQueue = mutable.ArrayDeque[MemTable]()
  private var numSSTables = 0

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

    tables.iterator.flatMap(_.get(key)).collectFirst {
      case MemTable.Tombstone => None
      case v => Some(v)
    }.flatten
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
    numSSTables += 1
    val filename = f"$numSSTables%06d"
    SSTableWriter.flush(memTable, filename)
  }
}

private object LSMTree {
  private val MinMemTableThresholdBytes = 1000 // Configure later
}

@main def main(): Unit = {
  val tree = new LSMTree()
  for (i <- 1 to 1000) {
    tree.put(s"Key$i", s"Value$i")
  }
}