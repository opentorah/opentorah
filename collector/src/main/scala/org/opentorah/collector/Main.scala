package org.opentorah.collector

import java.io.File
import java.net.URL
import org.opentorah.store.Store
import org.opentorah.tei.{Entity, EntityReference, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.{From, PrettyPrinter}
import org.slf4j.{Logger, LoggerFactory}
import scala.xml.Elem

object Main {

  LoggerFactory.getILoggerFactory.asInstanceOf[ch.qos.logback.classic.LoggerContext]
    .getLogger(Logger.ROOT_LOGGER_NAME).setLevel(ch.qos.logback.classic.Level.INFO)

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    doIt(
      fromUrl = From.file(new File(new File(docs, "store"), "store.xml")).url.get,
      siteRoot = docs
    )
  }

  def doIt(
    fromUrl: URL,
    siteRoot: File
  ): Unit = {
    logger.info("Reading store.")

    val store: Store = Store.read(fromUrl)
    val site = new Site(store, new SiteParameters(
      title = "Документы",
      author = "www.alter-rebbe.org",
      email = "dub@opentorah.org",
      faviconJpeg = "alter-rebbe",
      googleAnalyticsId = Some("UA-154490117-1"),
      navigationLinks = Seq(
        NavigationLink("/names", "Имена", Some(Viewer.Names)), // TODO doesn't show up in the Jekyll-generated pages (md)...
        NavigationLink("/collections", "Архивы", Some(Viewer.Collection)), // TODO doesn't show up in the Jekyll-generated pages (md)...
        NavigationLink("/help", "Помощь", Some(Viewer.Collection)),
        NavigationLink("/about", "О сайте", Some(Viewer.Collection))
      ),
      footerCol3 =
        // TODO when I use <p> instead of <span>, it gets styled by the TEI CSS - althought it is in the XHTML namespace?!
        <span>
          documents related to early Chabad history<br/>
          🄯 <a href="http://www.opentorah.org/" target={Viewer.Collection.name}>the Open Torah Project</a>
          <a href="http://creativecommons.org/licenses/by/4.0/" target={Viewer.Collection.name}>CC BY 4.0</a>
        </span>,
      homeTarget = Viewer.Collection
    ))

    logger.info("Checking store.")
    def findByRef(ref: String): Option[Entity] = store.entities.get.findByRef(ref)
    val errors: Seq[String] = site.references.flatMap(reference => checkReference(reference.value, findByRef))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    logger.info("Pretty-printing store.")
    for (entityHolder <- store.entities.get.by.get.stores)
      prettyPrint(entityHolder, Entity.toXmlElement(entityHolder.entity.copy(id = None)), Tei.prettyPrinter)
    prettyPrint(store)


    logger.info("Writing site.")
    Site.write(
      siteRoot,
      site
    )
  }

  private def prettyPrint(store: Store): Unit = {
    prettyPrint(store, Store.parsable.toXmlElement(store.asInstanceOf[Store.FromElement].element), Store.prettyPrinter)

    store match {
      case collection: Collection =>
        for (by <- collection.by; document <- by.stores; by <- document.by; teiHolder <- by.stores)
          prettyPrint(teiHolder, Tei.toXmlElement(teiHolder.tei), Tei.prettyPrinter)
      case _ =>
        for (by <- store.by; store <- by.stores) prettyPrint(store)
    }
  }

  private def prettyPrint(store: Store, toXml: => Elem, prettyPrinter: PrettyPrinter): Unit =
    for (fromUrl <- store.urls.fromUrl) if (Files.isFile(fromUrl)) Files.write(
      file = Files.url2file(fromUrl),
      content = prettyPrinter.renderXml(toXml)
    )

  private def checkReference(reference: EntityReference,  findByRef: String => Option[Entity]): Option[String] = {
    val name = reference.name
    reference.ref.fold[Option[String]](None) { ref =>
      if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
        findByRef(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
          if (named.entityType == reference.entityType) None
          else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
        }
      }
    }
  }
}
