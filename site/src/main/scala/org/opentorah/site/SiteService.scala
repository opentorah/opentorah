package org.opentorah.site

import net.logstash.logback.argument.{StructuredArgument, StructuredArguments}
import io.netty.handler.codec.http.HttpHeaderNames
import org.slf4j.Logger
import org.opentorah.util.{Effects, Files, Logging, Zhttp}
import org.opentorah.xml.{Element, Parser}
import zhttp.http._
import zio.duration.Duration
import zio.stream.ZStream
import zio.{Chunk, Task, ZEnv, ZIO}
import zio.blocking.Blocking
import java.io.File
import java.net.URL


abstract class SiteService[S <: Site[S]] extends Element[S]("site") with zio.App {

  // This is supposed to be set when running in Cloud Run
  private val serviceName: Option[String] = Option(System.getenv("K_SERVICE"))

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  private type ServiceEnvironment = ZEnv

  final def readSite(url: String): Task[S] = readSite(Files.string2url(url))

  private final def readSite(url: URL): Task[S] = {
    val siteFileUrl: URL = Files.fileInDirectory(url, "site.xml")
    val result: Parser[S] = for {
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl"))
      result <- parse(siteFileUrl)
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl - done"))
    } yield result
    Parser.toTask(result)
  }

  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  // TODO does this belong in the base class?
  protected def bucketName: String

  final override def run(args: List[String]): zio.URIO[ServiceEnvironment, zio.ExitCode] = {
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
    readSite(url).flatMap(_.build(withPrettyPrint))
  }

  private def upload(directoryPath: String, serviceAccountKey: String, dryRun: Boolean): Task[Unit] =
    Effects.effectTotal(new GoogleCloudStorageSynchronizer(
      serviceAccountKey = serviceAccountKey,
      bucketName = bucketName,
      bucketPrefix = "",
      directoryPath = directoryPath + "/",
      dryRun = dryRun
    ).sync())

  private def serve(urlString: Option[String]): zio.URIO[ServiceEnvironment, zio.ExitCode] = {
    def getParameter(name: String, defaultValue: String): String = scala.util.Properties.envOrNone(name).fold {
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

    Zhttp.start(port, routes(siteUrl))
  }

  private def routes(siteUrl: String): RHttpApp[ServiceEnvironment] = {
    // TODO move near the SiteReader
    var cachedSite: Option[S] = None
    def getSite: Task[S] = cachedSite.map(Task.succeed(_)).getOrElse(readSite(siteUrl).map { result =>
      cachedSite = Some(result)
      result
    })

    Effects.unsafeRun(getSite)

    def reset(request: Request): Task[UResponse] = for {
      _ <- Effects.effectTotal({
        info(request, "RST")
        cachedSite = None
      })
      _ <- getSite
    } yield Response.http(
      headers = List(Header.contentTypeTextPlain),
      content = Zhttp.textData("Site reset!")
    )

    def get(request: Request): ResponseM[Blocking, Throwable] = {
      val pathString: String = request.url.path.asString

      for {
        site <- getSite
        siteResponse <- site.resolveContent(pathString)
        result <- siteResponse.map(siteResponse => ZIO.succeed {
          val bytes: Array[Byte] = Zhttp.textBytes(siteResponse.content)

          Response.http(
            headers = List(
              // TODO: `Content-Type`(MediaType.unsafeParse(siteResponse.mimeType), Charset.`UTF-8`)
              Header(HttpHeaderNames.CONTENT_TYPE, siteResponse.mimeType),
              Header.contentLength(bytes.length.toLong)
              // TODO more headers!
            ),
            content = HttpData.fromStream(ZStream.fromChunk(Chunk.fromArray(bytes)))
          )
        }).getOrElse {
          val url: URL = Files.pathUnder(Files.string2url(siteUrl), pathString)
          for {
            static: Option[Response[Blocking, Throwable]] <- Zhttp.staticFile(url, Some(request))
          } yield static.getOrElse(Zhttp.notFound(pathString))
        }
      } yield result
    }

    Http.collectM[Request] {
      case request@Method.GET -> Root / "reset-cached-site" => reset(request)
      case request => time(request, get(request))
    }
  }

  private def time(
    request: Request,
    response: ResponseM[Blocking, Throwable]
  ): ResponseM[ServiceEnvironment, Throwable] =
    response.timed.mapEffect { case (duration: Duration, response: Response[Blocking, Throwable]) =>
      // TODO deal with the exhaustivity warning here!
      val (isWarning: Boolean, code: String) = response match {
        case response: Response.HttpResponse[Blocking, Throwable] => response.status match {
          case Status.OK           => (false, "GOT")
          case Status.NOT_FOUND    => (true , "NOT")
          case Status.NOT_MODIFIED => (false, "UNM")
          case _                   => (false, "---")
        }
        case _ => (true, "???")
      }

      val durationStr: String = SiteService.formatDuration(duration)
      val message: String = s"$code $durationStr ${request.url.path.asString}"

      if (isWarning)
        warning(request, message)
      else
        info   (request, message)

      response
    }

  // TODO move into Logging?

  private def info   (request: Option[Request], message: String): Unit = log(     request , message, "INFO"   )
  private def info   (request:        Request , message: String): Unit = log(Some(request), message, "INFO"   )
  private def info   (                          message: String): Unit = log(None         , message, "INFO"   )
  private def notice (request:        Request , message: String): Unit = log(Some(request), message, "NOTICE" )
  private def notice (                          message: String): Unit = log(None         , message, "NOTICE" )
  private def warning(request:        Request , message: String): Unit = log(Some(request), message, "WARNING")
  private def warning(                          message: String): Unit = log(None         , message, "WARNING")

  private final def logger: Logger = Site.logger

  private def log(request: Option[Request], message: String, severity: String): Unit = {
    val trace: Option[String] = request.flatMap(_.getHeaderValue("X-Cloud-Trace-Context")).map(_.split("/")(0))

    val arguments: Seq[StructuredArgument] =
      Seq(StructuredArguments.keyValue("severity", severity)) ++
      trace.toSeq.map(trace => StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace"))

    logger.info(message, arguments: _*)
  }
}

object SiteService {

  private def formatDuration(duration: Duration): String = {
    val millis: Long = duration.toMillis
    if (millis < 1000) s"$millis ms" else {
      val seconds: Float = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"
    }
  }
}
