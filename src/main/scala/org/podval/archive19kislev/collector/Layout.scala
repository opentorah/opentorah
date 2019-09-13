package org.podval.archive19kislev.collector

import java.io.File

object Layout {

  private final val docs: File = new File("docs").getAbsoluteFile

  private def docs(fileName: String): File = new File(docs, fileName)

  final val indexMd: File = docs("index.md")

  final val configYml: File = docs("_config.yml")

  // Note: also hard-coded in _layouts/tei.html (for 'names') and in 'index.md'!
  private final val collectionsDirectoryName: String = "collections"

  final val collections: File = docs(collectionsDirectoryName)

  def collections(name: String): File = new File(collections, name)

  final val collectionsXml: File = collections("collections.xml")

  private def url(ref: String): String = s"/$collectionsDirectoryName/$ref"

  final val namesFileDirectory: File = collections

  final val namesFileName: String = "names"

  final val namesUrl: String = url(s"$namesFileName.html")

  def collectionXml(directory: File): File = new File(directory, "collection.xml")

  def collectionUrl(name: String): String = url(s"$name/index.html")

  // Note: also hard-coded in _layouts/facsimile.html!
  private final val facsimilesDirectoryName: String = "facsimiles"

  def facsimiles(collectionDirectory: File): File = new File(collectionDirectory, facsimilesDirectoryName)

  // Note: also hard-coded in _layouts/tei.html!
  final val facsDirectoryName: String = "facs" // facsimile viewers

  def facs(collectionDirectory: File): File = new File(collectionDirectory, facsDirectoryName)

  final val teiDirectoryName: String = "tei"

  def tei(collectionDirectory: File): File = new File(collectionDirectory, teiDirectoryName)

  // Note: also hard-coded in _layouts/facsimile.html!
  final val docsDirectoryName: String = "documents" // wrappers for TEI XML

  def docs(collectionDirectory: File): File = new File(collectionDirectory, docsDirectoryName)

  def documentUrl(collectionDirectoryName: String, name: String): String =
    url(s"$collectionDirectoryName/${documentUrlRelativeToIndex(name)}")

  def documentUrlRelativeToIndex(name: String): String =  s"$docsDirectoryName/$name.html"
}
