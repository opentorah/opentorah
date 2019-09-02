package org.podval.archive19kislev.collector

import java.io.File

object Layout {
  final val docsRoot: File = new File("docs").getAbsoluteFile

  def docs(fileName: String): File = new File(docsRoot, fileName)

  final val namesFileName: String = "names"

  final val collectionsXmlFileName: String = "collections.xml"

  final val collectionXmlFileName: String = "collection.xml"

  final val indexMdFileName: String = "index.md"

  final val configYmlFileName: String = "_config.yml"

  object Collection {
    final val teiDirectoryName: String = "tei"

    final val facsimilesDirectoryName: String = "facsimiles"

    final val docsDirectoryName: String = "documents" // wrappers for TEI XML

    final val facsDirectoryName: String = "facs" // facsimile viewers
  }
}
