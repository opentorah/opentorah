package org.opentorah.collector

import java.io.File
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Entities, EntityHolder, Path, Store, WithPath}
import org.opentorah.util.Files
import org.opentorah.xml.{RawXml, XmlUtil}
import scala.xml.{Elem, Node}

final class Site(store: Store, val references: Seq[WithPath[EntityReference]]) {

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

  def resolve(url: String): Option[SiteFile] = {
    if (!url.startsWith("/")) None else {
      val parts: Seq[String] = Site.removePart(url).substring(1).split("/")
      if (parts.isEmpty) Some(new IndexObject(this).teiWrapperFile) else {
        val tail: Seq[String] = parts.tail
        parts.head match {
          case HierarchyObject.hierarchyDirectoryName =>
            HierarchyObject.resolve(this, Path.empty, store, tail)

          case CollectionObject.collectionsDirectoryName =>
            CollectionObject.resolve(this, tail)

          case EntityObject.namesDirectoryName =>
            EntityObject.resolve(this, tail)

          case file if parts.tail.isEmpty  =>
              val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
              fileName match {
                case "index" =>
                  SiteFile.resolve(extension, new IndexObject(this))

                case TreeIndexObject.collectionsFileName =>
                  SiteFile.resolve(extension, new TreeIndexObject(this))

                case NamesObject.namesFileName =>
                  SiteFile.resolve(extension, new NamesObject(this))

                case _ => None
              }

          case _ => None
        }
      }
    }
  }
}

object Site {

  def removePart(from: String): String = {
    val sharp = from.indexOf('#')
    if (sharp == -1) from else from.substring(0, sharp)
  }

  def addPart(url: Seq[String], part: String): Seq[String] =
    url.init :+ (url.last + "#" + part)

  val unpublished: Set[String] = Set("derzhavin6", "derzhavin7", "lna208",
    "niab5", "niab19", "niab24", "rnb203", "rnb211")

  def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1

  def referenceCollectionName(reference: WithPath[EntityReference]): String =
    reference.path.init.init.last.store.names.name

  def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

  // TODO this yuck is temporary :)

  def collectionReference(collection: WithPath[Collection]): String =
    collection.value.names.name

  def collectionTitle(collection: WithPath[Collection]): Seq[Node] =
    collection.value.title.fold[Seq[Node]](textNode(collectionReference(collection)))(_.xml)

  def collectionDescription(collection: WithPath[Collection]): Seq[Node] =
    Seq(<span>{collection.value.storeAbstract.get.xml}</span>) ++
      RawXml.getXml(collection.value.body)

  def collectionName(collection: WithPath[Collection]): String =
    fileName(collection.value)

  def collectionArchive(collection: WithPath[Collection]): Option[String] = {
    val reference = collectionReference(collection)
    val space = reference.lastIndexOf(' ')
    if (space == -1) None else Some(reference.substring(0, space))
  }

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

    Files.deleteFiles(new File(directory, EntityObject.namesDirectoryName))
    for (entity <- site.entities) writeSiteObject(new EntityObject(site, entity), directory)

    Files.deleteFiles(new File(directory, HierarchyObject.hierarchyDirectoryName))
    for (store <- site.stores) writeSiteObject(new HierarchyObject(site, store.path, store.value), directory)

    Files.deleteFiles(new File(directory, CollectionObject.collectionsDirectoryName))
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

    println("Writing reports.")

    writeReport(
      directory,
      name = "misnamed-entities",
      title = "Неправильно названные файлы с именами",
      content = site.entities.flatMap { entity =>
        val id: String = entity.id.get
        val expectedId: String = entity.name.replace(' ', '_')
        if (id == expectedId) None else Some(s"- '$id' должен по идее называться '$expectedId'")
      }
    )

    writeReport(
      directory,
      name = "no-refs",
      title = "Имена без атрибута /ref/",
      content =
        for (reference <- site.references.filter(_.value.ref.isEmpty)) yield
          "- " + reference.value.name.map(_.text.trim).mkString(" ") + " в " +
            referenceCollectionName(reference) + ":" +
            reference.path.last.store.names.name
    )
  }

  private def writeSiteObject(siteObject: SiteObject, directory: File): Unit = {
    writeSiteFile(siteObject.teiFile, directory)
    writeSiteFile(siteObject.teiWrapperFile, directory)
  }

  private final def writeSiteFile(siteFile: SiteFile, directory: File): Unit =
    Files.write(Site.file(directory, siteFile.url), siteFile.content)

  @scala.annotation.tailrec
  def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)

  def toXml(collection: WithPath[Collection]): Elem = {
    val url = CollectionObject.teiWrapperUrl(collection)
    // If I do this, parts of the line click to the names... {ref(url, collectionViewer, textNode(collectionReference(collection) + ": ") ++ collectionTitle(collection))}<lb/>
    <item>
      {ref(url, collectionReference(collection) + ": " +
        spacedText(collectionTitle(collection)))}<lb/>
      <abstract>{collection.value.storeAbstract.get.xml}</abstract>
    </item>
  }

  def ref(
    url: Seq[String],
    text: String,
    css: Option[String] = None
  ): Elem = <ref target={mkUrl(url)} rendition={css.orNull}>{text}</ref>

  def ref(
    url: Seq[String],
    text: Seq[Node]
  ): Elem = <ref target={mkUrl(url)}>{text}</ref>

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  private def refRoleRewriter(site: Site): Elem => Elem = elem =>
    if (elem.label != "ref") elem else {
      elem.attribute("target").map(_.text).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
        if (!target.startsWith("/")) elem else {
          val roleShouldBe: Option[String] = site.resolve(target).map(_.siteObject.viewer)
          val role: Option[String] = elem.attribute("role").map(_.text)
          if (roleShouldBe.isEmpty) println(s"did not resolve: $target")
          if (roleShouldBe.isDefined && role.isDefined && (role != roleShouldBe)) println(s"role discrepancy")
          if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
          else elem % scala.xml.Attribute(None, "role", textNode(roleShouldBe.get), scala.xml.Null)
        }
      }
    }

  def processTei(elem: Elem, site: Site): Elem =
    XmlUtil.rewriteElements(elem, refRoleRewriter(site))

  def withYaml(
    yaml: Seq[(String, String)],
    content: Seq[String] = Seq.empty
  ): String = {
    val result: Seq[String] =
      Seq("---") ++
      (for ((name, value) <- yaml) yield name + ": " + quote(value)) ++
      Seq("---") ++
      Seq("") ++ content

    result.mkString("", "\n", if (content.nonEmpty) "\n" else "")
  }

  private def quote(what: String): String = s"'$what'"

  private def writeReport(
    directory: File,
    name: String,
    title: String,
    content: Seq[String]
  ): Unit = Files.write(
    file = new File(new File(directory, "reports"), name + ".md"),
    withYaml(
      yaml = Seq("layout" -> "page", "title" -> title),
      content
    ))

  def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, textNode(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  def textNode(text: String): Node = new scala.xml.Text(text)

  def spacedText(nodes: Seq[Node]): String = nodes.map(spacedText).mkString("")
  def spacedText(node: Node): String = {
    val result = node match {
      case elem: Elem => (elem.child map (_.text)).mkString(" ")
      case node: Node => node.text
    }
    result
      .replace('\n', ' ')
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
  }
}
