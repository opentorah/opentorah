package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, Element, From, FromUrl, Parser, PrettyPrinter}
import java.io.File
import java.net.URL

/**
  * Directory of files.
  *
  * @tparam T type of into which the file is parsed
  * @tparam M type of the files list entry
  */
abstract class Directory[T <: AnyRef, M <: Directory.Entry](
  val directory: String,
  fileExtension: String,
  entry: Directory.EntryMaker[T, M]
) extends FromUrl.With {

  protected def loadFile(url: URL): T

  final protected def directoryUrl: URL = Files.subdirectory(fromUrl.url, directory)

  final def getFile(name: String): T =
    Cache.get[T](Files.fileInDirectory(directoryUrl, name + "." + fileExtension), loadFile)

  final def writeDirectory(): Unit = {
    val directory: File = Files.url2file(directoryUrl)
    val file: File = Files.url2file(listUrl)
    val files: Seq[String] = Files.filesWithExtensions(directory, fileExtension).sorted
    val result: Seq[M] = files.map(name => entry(name, getFile(name)))
    Files.write(
      file,
      PrettyPrinter.default.renderXml(directoryParsable.toXmlElement(result))
    )
  }

  def directoryEntries: Seq[M]

  final protected def readDirectory: Map[String, M] =
    Parser.parseDo(directoryParsable.parse(From.url(listUrl)))
      .map(entry => entry.name -> entry).toMap

  final protected def findByName(name: String, name2entry: Map[String, M]): Option[M] =
    Store.checkExtension(name, "html").flatMap(name2entry.get)

  private def listUrl: URL = Files.fileInDirectory(fromUrl.url, directory + "-list-generated-ng.xml")

  private val directoryParsable: Element[Seq[M]] = new Element[Seq[M]]("directory") {
    final override def parser: Parser[Seq[M]] = entry.all
    final override def antiparser: Antiparser[Seq[M]] = entry.toXmlSeq
  }
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

  private val directoryAttribute: Attribute[String] = Attribute("directory")

  val directory: Parser[String] = directoryAttribute.required

  def directoryToXml[T <: Directory[_, _]]: Antiparser[T] = directoryAttribute.toXml(_.directory)

  private val nameAttribute: Attribute[String] = Attribute("n")

  def fileName: Parser[String] = nameAttribute.required

  def fileNameToXml[T <: Entry]: Antiparser[T] = nameAttribute.toXml(_.name)
}
