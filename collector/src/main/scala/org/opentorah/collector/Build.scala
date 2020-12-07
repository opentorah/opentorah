package org.opentorah.collector

import org.opentorah.store.Store
import org.opentorah.util.Files
import org.opentorah.xml.From
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import java.net.URL

object Build {

  LoggerFactory.getILoggerFactory.asInstanceOf[ch.qos.logback.classic.LoggerContext]
    .getLogger(Logger.ROOT_LOGGER_NAME).setLevel(ch.qos.logback.classic.Level.INFO)

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val command: String = args(0)
    val siteRootPath: String = args(1)

    command match {
      case "verify"   => generateSite(siteRootPath, doPrettyPrint = true , doWrite = false)
      case "generate" => generateSite(siteRootPath, doPrettyPrint = false, doWrite = true )
      case "build"    => generateSite(siteRootPath, doPrettyPrint = true , doWrite = true )
      case "upload"   => GoogleCloudStorageSynchronizer.sync(
        serviceAccountKey = args(2),
        bucketName = "store.alter-rebbe.org",
        bucketPrefix = "",
        directoryPath = siteRootPath + "/",
        dryRun = (args.length > 3) && (args(3) == "dryRun")
      )
      case _ => throw new IllegalArgumentException(s"Unrecognized command [$command]")
    }
  }

  private def generateSite(
    siteRootPath: String,
    doPrettyPrint: Boolean,
    doWrite: Boolean
  ): Unit = {
    val siteRoot = new File(siteRootPath)
    val fromUrl: URL = From.file(Files.file(siteRoot, Seq("store", "store.xml"))).url.get // TODO Files.file2url?

    logger.info("Reading store.")

    val site: Site = new Site(Store.read(fromUrl), mkSiteParameters)

    logger.info("Checking references.")
    val errors: Seq[String] = site.checkReferences
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    if (doPrettyPrint) {
      logger.info("Pretty-printing store.")
      site.prettyPrintStore()
    }

    logger.info(if (doWrite) "Verifying and writing site." else "Verifying site.")

    Site.write(
      siteRoot,
      site,
      doWrite
    )
  }

  private def mkSiteParameters: SiteParameters = new SiteParameters(
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
  )
}
