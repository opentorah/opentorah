package org.podval.archive19kislev.collector

object Main {

  def main(args: Array[String]): Unit = {
    Collection.writeIndex("dubnov", "Дубнов")
    Collection.writeIndex("archive", "Архив 19 Кислева")
  }
}
