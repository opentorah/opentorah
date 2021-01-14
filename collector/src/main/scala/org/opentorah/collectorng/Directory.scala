package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, FromUrl}
import java.net.URL

/**
  * Directory of files.
  *
  * @tparam T type of into which the file is parsed
  * @tparam M type of the files list entry
  */
abstract class Directory[T <: AnyRef, M <: Directory.Entry, W <: AnyRef](
  val directory: String,
  fileExtension: String,
  entry: Directory.EntryMaker[T, M],
  wrapper: Map[String, M] => W
) extends FromUrl.With {

  final protected def directoryUrl: URL = Files.subdirectory(fromUrl.url, directory)

  private val listFile: ListFile[M, W] = new ListFile[M, W](
    url = Files.fileInDirectory(fromUrl.url, directory + "-list-generated.xml"),
    name = "directory",
    entry,
    wrapper = (entries: Seq[M]) => wrapper(entries.map(entry => entry.name -> entry).toMap)
  )

  final def writeDirectory(): Unit = listFile.write(
    Files.filesWithExtensions(Files.url2file(directoryUrl), fileExtension)
      .sorted
      .map(name => entry(name, getFile(name)))
  )

  final protected def getDirectory: W = listFile.get

  def directoryEntries: Seq[M]

  final def getFile(entry: M): T = getFile(entry.name)

  final def getFile(name: String): T =
    Cache.get[T](Files.fileInDirectory(directoryUrl, name + "." + fileExtension), loadFile)

  protected def loadFile(url: URL): T

  final protected def findByName(name: String, name2entry: Map[String, M]): Option[M] =
    Store.checkExtension(name, "html").flatMap(name2entry.get)
}

object Directory {

  abstract class Entry(
    val name: String
  ) extends Store {
    final override def names: Names = Names(name)
    final override def findByName(name: String): Option[Store] = None
  }

  abstract class EntryMaker[T, M <: Entry](elementName: String) extends Element[M](elementName) {
    def apply(name: String, content: T): M
  }

  val directoryAttribute: Attribute.Required[String] = Attribute("directory").required

  val fileNameAttribute: Attribute.Required[String] = Attribute("n").required
}
