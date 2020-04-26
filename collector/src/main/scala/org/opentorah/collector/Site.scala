package org.opentorah.collector

import java.io.File
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{Entities, EntityHolder, Store, WithPath}
import org.opentorah.util.Files

final class Site(val store: Store, val references: Seq[WithPath[EntityReference]]) {

  private val collections: Seq[WithPath[Collection]] = store.withPath[Collection](values = {
    case collection: Collection => Seq(collection)
    case _ => Seq.empty
  })

  def publishedCollections: Seq[WithPath[Collection]] = collections.filterNot(collection =>
    Site.unpublishedCollections.contains(Hierarchy.fileName(collection.value)))

  def findCollectionByName(collectionName: String): Option[WithPath[Collection]] =
    collections.find(collection => Hierarchy.fileName(collection.value) == collectionName)

  val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

  def findByRef(fileName: String): Option[Entity] =  store.entities.get.findByRef(fileName)

  def resolve(url: String): Option[SiteFile] =
    if (!url.startsWith("/")) None
    else SiteObject.resolve(this, Files.removePart(url).substring(1).split("/"))
}

object Site {

  val facsimileBucket: String = "http://facsimiles.alter-rebbe.org/facsimiles/"

  private val unpublishedCollections: Set[String] =
    Set("derzhavin6", "derzhavin7", "lna208", "niab5", "niab19", "niab24", "rnb203", "rnb211")

  // TODO with images on a separate website (facsimiles.alter-rebbe.org), this has to be re-worked...
  //  private def checkPages(): Unit = {
  //    val imageNames: Set[String] = Util.filesWithExtensions(directory = layout.facsimiles(directory), ".jpg").toSet
  //    imageNames.foreach(name => pageType(name, isPresent = true))
  //    val usedImages: Set[String] = pages.filter(_.isPresent).map(_.name).toSet
  //    val orphanImages: Seq[String] = (imageNames -- usedImages).toSeq.sorted
  //    val missingImages: Seq[String] = (usedImages -- imageNames).toSeq.sorted
  //    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  //    if (missingImages.nonEmpty) throw new IllegalArgumentException(s"Missing images: $missingImages")
  //  }

  def write(
    directory: File,
    site: Site
  ): Unit = {
    println("Writing site.")

    writeSiteObject(new IndexObject(site), directory)
    writeSiteObject(new TreeIndexObject(site), directory)
    writeSiteObject(new NamesObject(site), directory)

    Files.deleteFiles(new File(directory, EntityObject.directoryName))
    for (entity <- site.entities) writeSiteObject(new EntityObject(site, entity), directory)

    Files.deleteFiles(new File(directory, Hierarchy.directoryName))
    val stores: Seq[WithPath[Store]] = site.store.withPath[Store](values = {
      case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
      case store => Seq(store)
    })
    for (store <- stores) writeSiteObject(new HierarchyObject(site, store.path, store.value), directory)

    Files.deleteFiles(new File(directory, CollectionObject.directoryName))
    for (collection <- site.collections) writeSiteObject(new CollectionObject(site, collection), directory)

    for {
      collection <- site.collections
      document <- collection.value.documents
      teiHolder: TeiHolder <- document.teiHolders
    } {
      val documentObject = new DocumentObject(site, collection, document, teiHolder)
      writeSiteFile(documentObject.teiFile, directory)
      writeSiteFile(documentObject.teiWrapperFile, directory)
      writeSiteFile(documentObject.facsFile, directory)
    }

    Files.deleteFiles(new File(directory, ReportObject.directoryName))
    writeSiteObject(new MisnamedEntitiesReport(site), directory)
    writeSiteObject(new NoRefsReport(site), directory)
  }

  private def writeSiteObject(siteObject: SiteObject, directory: File): Unit = {
    writeSiteFile(siteObject.teiFile, directory)
    writeSiteFile(siteObject.teiWrapperFile, directory)
  }

  private final def writeSiteFile(siteFile: SiteFile, directory: File): Unit =
    Files.write(Files.file(directory, siteFile.url), siteFile.content)
}
