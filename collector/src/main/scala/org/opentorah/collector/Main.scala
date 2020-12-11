package org.opentorah.collector

import org.slf4j.{Logger, LoggerFactory}
import java.io.File

object Main {

  LoggerFactory.getILoggerFactory.asInstanceOf[ch.qos.logback.classic.LoggerContext]
    .getLogger(Logger.ROOT_LOGGER_NAME).setLevel(ch.qos.logback.classic.Level.INFO)
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  private def info(message: String): Unit = logger.info(message)

  private val bucketName: String = "store.alter-rebbe.org"

  def main(args: Array[String]): Unit = {
    val (command: String, siteRootPath: String) =
      if (args.nonEmpty) (args(0), args(1))
      // container no-args entry point: assume "serve", retrieve store url from the environment:
      else ("serve", getParameter("STORE", s"http://$bucketName/"))

    command match {
      case "verify"   => generateSite(siteRootPath, doPrettyPrint = true , doWrite = false)
      case "generate" => generateSite(siteRootPath, doPrettyPrint = false, doWrite = true )
      case "build"    => generateSite(siteRootPath, doPrettyPrint = true , doWrite = true )
      case "upload"   =>
        GoogleCloudStorageSynchronizer.sync(
          serviceAccountKey = args(2),
          bucketName,
          bucketPrefix = "",
          directoryPath = siteRootPath + "/",
          dryRun = (args.length > 3) && (args(3) == "dryRun")
        )
      case "serve"    =>
        Service.main(Array(
          siteRootPath,
          getParameter("PORT", "4000")
        ))
      case _ => throw new IllegalArgumentException(s"Unrecognized command [$command]")
    }
  }

  private def getParameter(name: String, defaultValue: String): String =Option(System.getenv(name)).fold {
    info(s"No value for '$name' in the environment; using default: '$defaultValue'")
    defaultValue
  }{ value =>
    info(s"Value    for '$name' in the environment: $value")
    value
  }

  private def generateSite(
    siteRootPath: String,
    doPrettyPrint: Boolean,
    doWrite: Boolean
  ): Unit = {
    info("Reading store.")
    val site: Site = new Site(new File(siteRootPath).toURI.toURL)

    if (doPrettyPrint) {
      info("Pretty-printing store.")
      site.prettyPrintStore()
    }

    info(if (doWrite) "Verifying and writing site." else "Verifying site.")
    site.write(doWrite)
  }
}
