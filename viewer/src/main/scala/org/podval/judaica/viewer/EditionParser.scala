package org.podval.judaica.viewer

import org.podval.judaica.viewer.ParseException.withMetadataFile

import java.io.File


object EditionParser {

  private final class ParseableEdition(override val work: Work, override val directory: File, index: File) extends Edition {

    override val names: Names = withMetadataFile(index)(Names(_))


    def storage: DirectoryStorage = storage_.get


    private[this] val storage_ = LazyLoad(withMetadataFile(index)(StorageParser.parseDirectoryStorage(work, _, directory)))
  }


  def parseEdition(work: Work, directory: File, index: File): Edition = new ParseableEdition(work, directory, index)
}
