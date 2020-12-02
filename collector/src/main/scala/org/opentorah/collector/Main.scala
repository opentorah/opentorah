package org.opentorah.collector

import java.io.File
import java.net.URL
import org.opentorah.store.Store
import org.opentorah.tei.{Entity, Tei}
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
      fromUrl = From.file(Files.file(docs, Seq("store", "store.xml"))).url.get, // TODO Files.file2url?
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
      faviconJpg = "alter-rebbe",
      googleAnalyticsId = Some("UA-154490117-1"),
      navigationLinks = Seq(
        NavigationLink("/names", "Имена", Some(Viewer.Names)),
        NavigationLink("/collections", "Архивы", Some(Viewer.Collection)),
        NavigationLink("/notes/help", "Помощь", Some(Viewer.Collection)),
        NavigationLink("/notes/about", "О сайте", Some(Viewer.Collection))
      ),
      footerCol3 =
        // TODO when I use <p> instead of <span>, it gets styled by the TEI CSS - althought it is in the XHTML namespace?!
        <span>
          documents related to early Chabad history<br/>
          🄯 <a href="http://www.opentorah.org/" target={Viewer.Collection.name}>the Open Torah Project</a>
          <a href="http://creativecommons.org/licenses/by/4.0/" target={Viewer.Collection.name}>CC BY 4.0</a>
        </span>,
      homeTarget = Viewer.Collection,
      githubUsername = None, // Some("opentorah"),
      twitterUsername = None
    ))

    logger.info("Checking store.")
    val errors = site.references.check(store.entities.get.findByRef)
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
}
