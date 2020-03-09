package org.opentorah.archive.collector

import java.io.File

final class Layout(val docs: File) {

  private def docs(fileName: String): File = new File(docs, fileName)

  val store: File = new File(docs, "store")

  private def store(fileName: String): File = new File(store, fileName)

  val indexFileName: String = "index"
  val collectionsFileName: String = "collections"

  // TODO Note: also hard-coded in 'index.xml'!
  private val collectionsDirectoryName: String = "collections"

  val collections: File = docs(collectionsDirectoryName)

  val storeCollections: File = store(collectionsDirectoryName)

  private def url(ref: String): String = s"/$collectionsDirectoryName/$ref"

  val namesDirectoryName: String = "names"

  val namesDirectory: File = docs(namesDirectoryName)

  val storeNamesDirectory: File = store(namesDirectoryName)

  val namesFileDirectory: File = docs

  val namesFileName: String = "names"

  val namesListsFileName: String = "names-lists"

  val namesUrl: String = s"/$namesFileName.html"

  def namedUrl(id: String): String = s"/$namesDirectoryName/$id.html"

  def namedInTheListUrl(id: String): String = s"/$namesFileName.html#$id"

  val collectionFileName: String = "collection"

  // TODO abstract out collectionIndexFileName: String = "index"; mention hard-coded instances.
  def collectionUrl(name: String): String = url(s"$name/index.html")

  // TODO Note: also hard-coded in _layouts/tei.html!
  val facsDirectoryName: String = "facs" // facsimile viewers

  val documentsDirectoryName: String = "documents"

  def facs(collectionDirectory: File): File = new File(collectionDirectory, facsDirectoryName)

  val teiDirectoryName: String = "tei"

  def tei(collectionDirectory: File): File = new File(collectionDirectory, teiDirectoryName)

  private val docsDirectoryName: String = "documents" // wrappers for TEI XML

  def docs(collectionDirectory: File): File = new File(collectionDirectory, docsDirectoryName)

  def documentUrl(collectionDirectoryName: String, name: String): String =
    url(s"$collectionDirectoryName/${documentUrlRelativeToIndex(name)}")

  def documentUrlRelativeToIndex(name: String): String =  s"$docsDirectoryName/$name.html"

  val reportsDirectory: File = docs("reports")

  def reportFile(name: String) = new File(reportsDirectory, name + ".md")
}
