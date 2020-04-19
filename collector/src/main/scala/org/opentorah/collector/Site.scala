package org.opentorah.collector

import java.io.File
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Entities, EntityHolder, Store, WithPath}
import org.opentorah.util.Files

final class Site(val store: Store, val references: Seq[WithPath[EntityReference]]) {

  val stores: Seq[WithPath[Store]] = store.withPath[Store](values = {
    case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
    case store => Seq(store)
  })

  val collections: Seq[WithPath[Collection]] = store.withPath[Collection](values = {
    case collection: Collection => Seq(collection)
    case _ => Seq.empty
  })

  def findCollectionByName(collectionName: String): Option[WithPath[Collection]] =
    collections.find(collection => Site.fileName(collection.value) == collectionName)

  val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

  def findByRef(fileName: String): Option[Entity] =  store.entities.get.findByRef(fileName)

  val entitiesLists: Seq[EntitiesList] = store.entities.get.lists.filterNot(_.isEmpty)

  def resolve(url: String): Option[SiteFile] =
    if (!url.startsWith("/")) None
    else SiteObject.resolve(this, Files.removePart(url).substring(1).split("/"))
}

object Site {

  val facsimileBucket: String = "http://facsimiles.alter-rebbe.org/facsimiles/"

  val unpublishedCollections: Set[String] =
    Set("derzhavin6", "derzhavin7", "lna208", "niab5", "niab19", "niab24", "rnb203", "rnb211")

  def loadTei(tei: String): String =
    s"<script type='module'>import loadTei from '/js/tei.js'; loadTei('$tei');</script>"

  //val root: Selector = new Selector(new Names(Seq(Name("Корень", Language.Russian))))

  def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1

  def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

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
    for (store <- site.stores) writeSiteObject(new HierarchyObject(site, store.path, store.value), directory)

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
