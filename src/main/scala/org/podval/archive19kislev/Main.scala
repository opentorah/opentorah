package org.podval.archive19kislev


object Main {

  def main(args: Array[String]): Unit = {
    val dubnovCollection = new Collection("dubnov", "Дубнов")
    dubnovCollection.writeIndex()

//    val archiveCollection = new Collection("archive", "Архив 19 Кислева")
//    archiveCollection.writeIndex()
  }
}
