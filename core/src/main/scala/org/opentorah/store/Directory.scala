package org.opentorah.store

import org.opentorah.metadata.{Named, Names}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ElementsTo, FromUrl, Parser}
import zio.ZIO
import java.net.URL

/**
 * Directory of files.
 *
 * @tparam T type of into which the file is parsed
 * @tparam M type of the files list entry
 */
abstract class Directory[
  T <: AnyRef,
  M <: Directory.Entry,
  W <: Directory.Wrapper[M]
](
  val directory: String,
  fileExtension: String,
  entryMaker: Directory.EntryMaker[T, M],
  wrapper: Map[String, M] => W
) extends Stores[M], FromUrl.With:

  private val listFile: ListFile[M, W] = ListFile[M, W](
    url = Files.fileInDirectory(fromUrl.url, directory + "-list-generated.xml"),
    name = "directory",
    entryMaker,
    wrapper = (entries: Seq[M]) => wrapper(entries.map(entry => entry.name -> entry).toMap)
  )

  override def findByName(name: String): Parser[Option[M]] = getDirectory.map(_.findByName(name))

  override def stores: Parser[Seq[M]] = getDirectory.map(_.stores)

  // TODO rename getWrapper?
  private final def getDirectory: Parser[W] = listFile.get

object Directory:

  abstract class Wrapper[M <: Store](name2entry: Map[String, M]):
    final def stores: Seq[M] = name2entry.values.toSeq

    final def findByName(name: String): Option[M] = name2entry.get(name)

  abstract class Entry(
    val name: String
  ) extends Terminal:
    final override val names: Names = Names(name)

  trait EntryMaker[T, M <: Entry] extends ElementsTo[M]:
    def apply(name: String, content: T): Parser[M]

  val directoryAttribute: Attribute.Required[String] = Attribute("directory").required

  val fileNameAttribute: Attribute.Required[String] = Attribute("n").required
