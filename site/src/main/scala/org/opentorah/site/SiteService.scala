package org.opentorah.site

import cats.data.OptionT
import fs2.Stream
import net.logstash.logback.argument.{StructuredArgument, StructuredArguments}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.{Charset, Headers, HttpRoutes, MediaType, Response, Request, StaticFile, Uri}
import org.slf4j.{Logger, LoggerFactory}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Element, Parser}
import org.typelevel.ci.CIString
import zio.duration.Duration
import zio.interop.catz._
import zio.{RIO, Task, URIO, ZEnv}
import java.io.File
import java.net.URL
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.jdk.CollectionConverters.SeqHasAsJava

// see https://zio.dev/docs/howto/interop/with-cats-effect
abstract class SiteService[S <: Site[S]] extends Element[S]("site") with zio.interop.catz.CatsApp {

  // This is supposed to be set when running in Cloud Run
  private val serviceName: Option[String] = Option(System.getenv("K_SERVICE"))

  LoggerFactory.getILoggerFactory match {
    case loggerContext: ch.qos.logback.classic.LoggerContext =>
      SiteService.configureLogback(loggerContext, inCloudRun = serviceName.isDefined)

    case _ =>
  }

  private type ServiceEnvironment = ZEnv

  private type ServiceTask[+A] = RIO[ServiceEnvironment, A]

  private final def readSite(url: URL): Task[S] = readSiteFile(Files.fileInDirectory(url, "site.xml"))

  private final def readSiteFile(url: URL): Task[S] = Parser.toTask(
    Effects.effect(logger.info(s"Reading site from $url")) *>
    parse(url)
  )

  // TODO remove?
//  private final def doReadSiteFile(file: File): S = Parser.run(readSiteFile(file))
//  private final def readSiteFile(file: File): Parser[S] = parse(Files.file2url(file))

  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  // TODO does this belong in the base class?
  protected def bucketName: String

  final override def run(args: List[String]): URIO[ServiceEnvironment, zio.ExitCode] = {
    if (args.isEmpty) serve(urlString = None) else args.head match {
      case "serveRemoteSite"         => serve(urlString = None)
      case "serveSite"               => serve(urlString = args.lift(1))
      case "buildAndPrettyPrintSite" => build(urlString = args(1), withPrettyPrint = true)
      case "buildSite"               => build(urlString = args(1), withPrettyPrint = false)
      case "uploadSite"              => upload(directoryPath = args(1), serviceAccountKey = args(2), dryRun = false)
      case "uploadSiteDryRun"        => upload(directoryPath = args(1), serviceAccountKey = args(2), dryRun = true )
    }
  }.exitCode

  private def build(urlString: String, withPrettyPrint: Boolean): Task[Unit] = {
    val url: URL = Files.file2url(new File(urlString))
    readSite(url) >>= (_.build(withPrettyPrint))
  }

  private def upload(directoryPath: String, serviceAccountKey: String, dryRun: Boolean): Task[Unit] =
    Effects.effectTotal(new GoogleCloudStorageSynchronizer(
      serviceAccountKey = serviceAccountKey,
      bucketName = bucketName,
      bucketPrefix = "",
      directoryPath = directoryPath + "/",
      dryRun = dryRun
    ).sync())

  private def serve(urlString: Option[String]): URIO[ServiceEnvironment, zio.ExitCode] = {
    def getParameter(name: String, defaultValue: String): String = Option(System.getenv(name)).fold {
      info(s"No value for '$name' in the environment; using default: '$defaultValue'")
      defaultValue
    }{ value =>
      info(s"Value    for '$name' in the environment: $value")
      value
    }

    val siteUrl: String = if (urlString.nonEmpty) {
      val result: String = urlString.get
      info(s"siteUri argument supplied: $result")
      result
    } else getParameter("STORE", s"http://$bucketName/") // TODO switch to https

    val port: Int = getParameter("PORT", "8080").toInt

    warning(s"serviceName=$serviceName")

    stream(port, siteUrl)
      .compile
      .drain
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err"))
      .exitCode
  }

  private def stream(port: Int, siteUrl: String): Stream[ServiceTask, Nothing] = {
    import org.http4s.implicits._

    val executionContext: ExecutionContextExecutor = ExecutionContext.global
    val httpApp = routes(siteUrl).orNotFound

    val result = for {
      _ <- BlazeClientBuilder[ServiceTask](executionContext).stream
      exitCode <- BlazeServerBuilder[ServiceTask](executionContext)
        // To be accessible when running in a docker container the server
        // must bind to all IPs, not just 127.0.0.1:
        .bindHttp(port, "0.0.0.0")
        .withWebSockets(false)
        .withHttpApp(httpApp)
        .serve : Stream[ServiceTask, cats.effect.ExitCode]
    } yield exitCode

    // TODO Scala 3.0.2 requires this cast (there is some ascription workaround...), but Scala 2 does not?!
    (result.asInstanceOf[Stream[ServiceTask, cats.effect.ExitCode]])
      .drain
  }

  private def routes(siteUrl: String): HttpRoutes[ServiceTask] = {
    val dsl = new Http4sDsl[ServiceTask] {}
    import dsl._

    val siteUri: Uri = Uri.unsafeFromString(siteUrl)

    // TODO move near the SiteReader
    var cachedSite: Option[S] = None
    def getSite: Task[S] = cachedSite.map(Task.succeed(_)).getOrElse(readSite(SiteService.toUrl(siteUri)).map { result =>
      cachedSite = Some(result)
      result
    })

    Effects.unsafeRun(getSite)

    def get(request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = {
      // TODO if I add a parameter list (implicit F: Sync[ServiceTask]) and use F.pure and F.suspend, I get
      //   ambiguous implicit values:
      //   both method taskConcurrentInstance in trait CatsEffectInstances1 of type
      //     [R]cats.effect.Concurrent[[β$18$]zio.ZIO[R,Throwable,β$18$]]
      //   and value F of type cats.effect.Sync[org.opentorah.collector.Service.ServiceTask]
      //   match expected type cats.effect.Sync[org.opentorah.collector.Service.ServiceTask]
      //val F: Sync[ServiceTask] = taskConcurrentInstance

      val path: Seq[String] = Files.splitAndDecodeUrl(request.uri.path.toString)
      val urlStr: String = Files.mkUrl(path)

      OptionT[ServiceTask, Response[ServiceTask]](for {
        site <- getSite
        siteResponse <- site.resolveContent(path)
        result = siteResponse.map { siteResponse =>
          val bytes: Array[Byte] = siteResponse.content.getBytes
          Response[ServiceTask](
            headers = Headers(
              `Content-Type`(MediaType.unsafeParse(siteResponse.mimeType), Charset.`UTF-8`),
              `Content-Length`.unsafeFromLong(bytes.length)
              // TODO more headers!
            ),
            //  // TODO Scala 3.0.2 requires this cast (but there is some)
            // TODO Scala 3.0.2 requires this cast, but Scala 2 does not?!
            body = Stream.emits(bytes).asInstanceOf[Stream[ServiceTask, Byte]]
          )
        }
      } yield result)
        .orElse {
          val path: Uri.Path = request.uri.path
          val implied: Uri.Path = if (path.endsWithSlash) path/Uri.Path.Segment("index.html") else path
          val uri: Uri = siteUri.copy(path = siteUri.path.addSegments(implied.segments))
          StaticFile.fromURL[ServiceTask](SiteService.toUrl(uri), Some(request))
        }
        .getOrElseF(NotFound(s"Not found: $urlStr"))
        .timed.mapEffect { case (duration: Duration, response: Response[ServiceTask]) =>
          val durationStr: String = SiteService.formatDuration(duration)

          if (response.status.isInstanceOf[NotFound.type])
            warning(request, s"NOT $durationStr $urlStr")
          else
            info   (request, s"GOT $durationStr $urlStr")

          response
      }
    }

    HttpRoutes.strict[ServiceTask] {
      case request@GET -> Root / "reset-cached-site" => for {
        _ <- Effects.effectTotal(info(request, "RST"))
        _ <- Effects.effectTotal({cachedSite = None})
        _ <- getSite
        result <- Ok("Site reset!")
      } yield result

      case request@GET -> _ => get(request)
    }
  }

  private def info   (request: Option[Request[ServiceTask]], message: String): Unit = log(request, message, "INFO"   )
  private def info   (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "INFO"   )
  private def info   (                               message: String): Unit = log(None         , message, "INFO"   )
  //  private def notice (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "NOTICE" )
  //  private def notice (                               message: String): Unit = log(None         , message, "NOTICE" )
  private def warning(request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "WARNING")
  private def warning(                               message: String): Unit = log(None         , message, "WARNING")

  private final def logger: Logger = Site.logger

  private def log(request: Option[Request[ServiceTask]], message: String, severity: String): Unit = {
    val trace: Option[String] = request.flatMap(_
      .headers.get(CIString("X-Cloud-Trace-Context"))
      .map(_.toString.split("/")(0))
    )

    val arguments: Seq[StructuredArgument] =
      Seq(StructuredArguments.keyValue("severity", severity)) ++
        trace.toSeq.map(trace => StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace"))

    logger.info(message, arguments: _*)
  }
}

object SiteService {

  private def configureLogback(loggerContext: ch.qos.logback.classic.LoggerContext, inCloudRun: Boolean): Unit = {
    val rootLogger: ch.qos.logback.classic.Logger = loggerContext.getLogger(Logger.ROOT_LOGGER_NAME)

    if (inCloudRun) {
      val statusManager = loggerContext.getStatusManager
      if (statusManager != null) statusManager.add(new ch.qos.logback.core.status.InfoStatus("Configuring logger", loggerContext))

      val encoder = new net.logstash.logback.encoder.LogstashEncoder
      // Ignore default logging fields
      encoder.setExcludeMdcKeyNames(List("timestamp", "version", "logger", "thread", "level", "levelValue").asJava)

      val consoleAppender = new ch.qos.logback.core.ConsoleAppender[ch.qos.logback.classic.spi.ILoggingEvent]
      consoleAppender.setName("jsonConsoleAppender")
      consoleAppender.setContext(loggerContext)
      consoleAppender.setEncoder(encoder)

      rootLogger.detachAndStopAllAppenders()
      rootLogger.addAppender(consoleAppender)
    }

    rootLogger.setLevel(ch.qos.logback.classic.Level.INFO)
  }

  private def toUrl(uri: Uri): URL = new URL(uri.toString)

  private def formatDuration(duration: Duration): String = {
    val millis: Long = duration.toMillis
    if (millis < 1000) s"$millis ms" else {
      val seconds: Float = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"
    }
  }
}
