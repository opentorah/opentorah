package org.opentorah.collector

import org.opentorah.store.Path
import org.opentorah.util.Files

final class RootSiteObject(site: Site) extends SiteObject(site) {
  // TODO generalize and move into SiteObject
  def resolve(parts: Seq[String]): Option[SiteFile] = {
    if (parts.isEmpty) Some(new IndexObject(site).htmlFile) else {
      val tail: Seq[String] = parts.tail
      parts.head match {
        case Hierarchy       .directoryName => HierarchyObject .resolve(site, Path.empty, site.store, tail)
        case CollectionObject.directoryName => CollectionObject.resolve(site, tail)
        case EntityObject    .directoryName => new NamesObject  (site).resolve(tail)
        case ReportsObject   .directoryName => new ReportsObject(site).resolve(tail)
        case NotesObject     .directoryName => new NotesObject  (site).resolve(tail)

        case file if parts.tail.isEmpty =>
          val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
          simpleSubObjects
            .find(_.fileName == fileName)
            .flatMap(k => SimpleSiteObject.resolve(extension, k))
            // Assume that this is a collection reference:
            .orElse(CollectionObject.resolve(site, parts))

        // Assume that this is a collection reference:
        case _ => CollectionObject.resolve(site, parts)
      }
    }
  }

  def simpleSubObjects: Seq[SimpleSiteObject] = Seq(
    new IndexObject(site),
    new TreeIndexObject(site),
    new NamesObject(site)
  )
}


