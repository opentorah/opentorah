package org.podval.judaica.viewer

import org.podval.judaica.viewer.ParseException.withMetadataFile
import org.podval.judaica.xml.Xml.Ops

import java.io.File


object EditionParser {

  private final class ParseableEdition(
    override val work: Work,
    override val names: Names,
    override val language: String,
    override val directory: File,
    index: File) extends Edition
  {
    def storage: DirectoryStorage = storage_.get


    private[this] val storage_ = LazyLoad(withMetadataFile(index)(StorageParser.parseDirectoryStorage(work, _, directory)))
  }



  def parseEdition(work: Work, directory: File, index: File): Edition = {
    withMetadataFile(index) { xml =>
      val names = Names(xml)
      val language = xml.attributeOption("language").getOrElse("he")
      new ParseableEdition(work, names, language, directory, index)
    }
  }
}
