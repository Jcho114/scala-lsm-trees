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
  private val listOfSSTables = mutable.ArrayDeque[SSTable]()
  private val flushWorkerIsRunning = new AtomicBoolean(true)
  private var nextGenerationId = 0
  private var filePath = ""

  /**
   * Background thread that flushes full MemTables residing in memory
   * to SSTables residing on disk
   */
  private val flushWorker = new Thread(() => {
    var running = true

    while (running) {
      val memTableOpt = flushableMemTableQueue.synchronized {
        while(flushableMemTableQueue.isEmpty && flushWorkerIsRunning.get()) {
          flushableMemTableQueue.wait()
        }

        if (flushableMemTableQueue.isEmpty && !flushWorkerIsRunning.get()) {
          None
        } else {
          Some(flushableMemTableQueue.removeHead())
        }
      }

      memTableOpt match {
        case Some(memTable) => flushMemTableToSSTable(memTable)
        case None => running = false
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

    val recoveredSSTables = mutable.ArrayDeque.empty[SSTable]
    val recoveredFlushables = mutable.ArrayBuffer.empty[MemTable]
    var recoveredActive: Option[MemTable] = None
    var recoveredNextGenerationId = 0

    try {
      val files = Option(dir.listFiles())
        .getOrElse(Array.empty[File])
        .filter(_.isFile)

      val sstFiles = files
        .filter(_.getName.matches(SSTable.Regex))
        .sortBy(_.getName.stripSuffix(".sst").toInt)
      val sstIds = sstFiles.map(file => parseGenerationId(file.getName, ".sst"))
      val recoveredSstIds = sstIds.toSet

      for (sstFile <- sstFiles) {
        val sst = SSTable.fromDisk(sstFile.getPath)
        recoveredSSTables.prepend(sst)
      }

      val walFiles = files
        .filter(_.getName.matches(WriteAheadLog.Regex))
        .sortBy(file => parseGenerationId(file.getName, ".wal"))
      val unrecoveredWalFiles = walFiles.filterNot(file =>
        recoveredSstIds.contains(parseGenerationId(file.getName, ".wal"))
      )

      val walIds = walFiles.map(file => parseGenerationId(file.getName, ".wal"))
      val existingIds = sstIds ++ walIds
      recoveredNextGenerationId = if (existingIds.isEmpty) 0 else existingIds.max + 1

      for (walFile <- unrecoveredWalFiles.dropRight(1)) {
        recoveredFlushables += recoverMemTableFromWal(walFile)
      }

      recoveredActive = unrecoveredWalFiles.lastOption match {
        case Some(walFile) =>
          val memTable = recoverMemTableFromWal(walFile)
          if (memTable.estimatedSizeInBytes() >= LSMTree.MinMemTableThresholdBytes) {
            recoveredFlushables += memTable
            None
          } else {
            Some(memTable)
          }
        case None => None
      }
    } catch {
      case e: Throwable =>
        recoveredActive.foreach(_.close())
        recoveredFlushables.foreach(_.close())
        recoveredSSTables.foreach(_.close())
        throw e
    }

    nextGenerationId = recoveredNextGenerationId
    recoveredSSTables.foreach(listOfSSTables.append)
    recoveredFlushables.foreach(addMemTableToFlushableQueue)
    activeMemTable = recoveredActive.getOrElse(newActiveMemTable())
    flushWorker.start()
  }

  /**
   * Helper function to append active MemTable to flush queue
   * @param memTable MemTable to append to queue
   */
  private def addMemTableToFlushableQueue(memTable: MemTable): Unit = {
    flushableMemTableQueue.synchronized {
      flushableMemTableQueue.append(memTable)
      flushableMemTableQueue.notifyAll()
    }
  }

  private def newActiveMemTable(): MemTable = {
    val id = allocateGenerationId()
    val wal = new WriteAheadLog(f"$filePath/$id%06d.wal")
    new MemTable(id, wal)
  }

  private def createNewActiveMemTable(): Unit = {
    activeMemTable = newActiveMemTable()
  }

  private def recoverMemTableFromWal(walFile: File): MemTable = {
    val id = parseGenerationId(walFile.getName, ".wal")
    val wal = new WriteAheadLog(walFile.getPath)
    try {
      new MemTable(id, wal)
    } catch {
      case e: Throwable =>
        wal.close()
        throw e
    }
  }

  private def parseGenerationId(filename: String, suffix: String): Int =
    filename.stripSuffix(suffix).toInt

  /**
   * Put key-value pair to lsm-tree
   *
   * @param key   Key
   * @param value Value
   * @return True if put succeeds otherwise False
   */
  def put(key: String, value: String): Boolean = {
    if (value == MemTable.Tombstone) return false // Do not allow user to delete with tombstone
    activeMemTable.put(key, value)
    if (activeMemTable.estimatedSizeInBytes() >= LSMTree.MinMemTableThresholdBytes) {
      addMemTableToFlushableQueue(activeMemTable)
      createNewActiveMemTable()
    }
    true
  }

  /**
   * Get corresponding value from lsm-tree if key exists
   * @param key Key
   * @return Some value if key exists and None otherwise
   */
  def get(key: String): Option[String] = {
    val (active, queued, ssts) = flushableMemTableQueue.synchronized {
      (
        activeMemTable,
        flushableMemTableQueue.toList.reverse,
        listOfSSTables.toList
      )
    }
    val tables = Iterator.single(active) ++ queued.iterator

    for (table <- tables.iterator) {
      table.get(key) match {
        case Some(MemTable.Tombstone) => return None
        case Some(v) => return Some(v)
        case None =>
      }
    }

    for (sst <- ssts) {
      sst.findEntry(key) match {
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
  def delete(key: String): Option[String] = {
    val res = activeMemTable.delete(key)
    if (activeMemTable.estimatedSizeInBytes() >= LSMTree.MinMemTableThresholdBytes) {
      addMemTableToFlushableQueue(activeMemTable)
      createNewActiveMemTable()
    }
    res
  }

  /**
   * Function to close the lsm-tree process
   */
  def close(): Unit = {
    flushWorkerIsRunning.set(false)
    flushableMemTableQueue.synchronized {
      flushableMemTableQueue.notifyAll()
    }
    flushWorker.join()
  }

  /**
   * Flushes MemTable residing in memory to SSTable file on disk
   */
  private def flushMemTableToSSTable(memTable: MemTable): Unit = {
    val filename = f"$filePath/${memTable.id}%06d.sst"
    val sst = SSTable.fromMemTable(memTable, filename)
    memTable.wal.foreach(wal => wal.delete())
    listOfSSTables.prepend(sst)
  }

  private def allocateGenerationId(): Int = {
    val id = nextGenerationId
    nextGenerationId += 1
    id
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