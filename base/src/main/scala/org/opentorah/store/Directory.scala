package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Elements, FromUrl, Parser}
import zio.ZIO
import java.net.URL

/**
 * Directory of files.
 *
 * @tparam T type of into which the file is parsed
 * @tparam M type of the files list entry
 */
abstract class Directory[T <: AnyRef, M <: Directory.Entry, W <: Directory.Wrapper[M]](
  val directory: String,
  fileExtension: String,
  entryMaker: Directory.EntryMaker[T, M],
  wrapper: Map[String, M] => W
) extends Store.NonTerminal with FromUrl.With {

  final def directoryUrl: URL = Files.subdirectory(fromUrl.url, directory)

  private val listFile: ListFile[M, W] = new ListFile[M, W](
    url = Files.fileInDirectory(fromUrl.url, directory + "-list-generated.xml"),
    name = "directory",
    entryMaker,
    wrapper = (entries: Seq[M]) => wrapper(entries.map(entry => entry.name -> entry).toMap)
  )

  final def writeDirectory(): Caching.Parser[Unit] =
    ZIO.foreach(
      Files.filesWithExtensions(Files.url2file(directoryUrl), fileExtension).sorted
    )(name => getFile(name).flatMap(file=> entryMaker(name, file)))
      .map(listFile.write)

  // Note: Entities, Notes and Collection derive from this;
  // in Entities and Notes findByName() delegates to findByNameInDirectory(),
  // but I can't make it a default and remove overrides from Entity and Note,
  // because Entity's findByName() needs to return Entity, not just Store,
  // but if I type this default with M, Collection's findByName(), which returns
  // document facet, becomes invalid...
  final def findByNameInDirectory(
    name: String,
    allowedExtension: String = "html",
    assumeAllowedExtension: Boolean = false
  ): Caching.Parser[Option[M]] = Stores.findByNameWithExtension(
    name = name,
    findByName = name => getDirectory.map(_.get(name)),
    allowedExtension = allowedExtension,
    assumeAllowedExtension = assumeAllowedExtension
  )

  final def getDirectory: Caching.Parser[W] = listFile.get

  final def directoryEntries: Caching.Parser[Seq[M]] = getDirectory.map(_.entries)

  final def getFile(entry: M): Caching.Parser[T] = getFile(entry.name)

  private def getFile(name: String): Caching.Parser[T] = Caching.getCached[T](fileUrl(name), loadFile)

  private def fileUrl(name: String): URL = Files.fileInDirectory(directoryUrl, name + "." + fileExtension)

  final def writeFile(entry: M, content: String): Unit = Files.write(
    file = Files.url2file(fileUrl(entry.name)),
    content
  )

  protected def loadFile(url: URL): Parser[T]
}

object Directory {

  abstract class Wrapper[M <: Store](name2entry: Map[String, M]) {
    final def entries: Seq[M] = name2entry.values.toSeq

    final def get(name: String): Option[M] = name2entry.get(name)
  }

  abstract class Entry(
    override val name: String
  ) extends Store.Terminal {
    final override val names: Names = Names(name)
  }

  trait EntryMaker[T, M <: Entry] extends Elements[M] {
    def apply(name: String, content: T): Parser[M]
  }

  val directoryAttribute: Attribute.Required[String] = Attribute("directory").required

  val fileNameAttribute: Attribute.Required[String] = Attribute("n").required
}
