import scala.collection.mutable

/**
 * LSM Tree main driver class
 */
class LSMTree {
  private var activeMemTable = new MemTable()
  private val flushableMemTableQueue = mutable.ArrayDeque[MemTable]()

  /**
   * Background thread that flushes full MemTables residing in memory
   * to SSTables residing on disk
   */
  private val worker = new Thread(() => {
    while (true) {
      val memTable = flushableMemTableQueue.synchronized {
        while(flushableMemTableQueue.isEmpty) flushableMemTableQueue.wait()
        flushableMemTableQueue.head
      }

      memTable.flushToSSTable()
      flushableMemTableQueue.synchronized {
        flushableMemTableQueue.removeHead()
      }
    }
  })
  worker.start()

  /**
   * Put key-value pair to lsm-tree
   * @param key Key
   * @param value Value
   * @return True if put succeeds otherwise False
   */
  def put(key: String, value: String): Boolean = {
    if (value == MemTable.Tombstone) return false // Do not allow user to delete with tombstone
    activeMemTable.put(key, value)
    if (activeMemTable.sizeInBytes() >= LSMTree.MinMemTableThresholdBytes) {
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
}

private object LSMTree {
  private val MinMemTableThresholdBytes = 1000 // Configure later
}

@main def main(): Unit = {
  val tree = new LSMTree()
  tree.put("Key1", "Value1")
  println(tree.get("Key1"))
  tree.put("Key1", "Value2")
  println(tree.get("Key1"))
  tree.delete("Key1")
  println(tree.get("Key1"))
}