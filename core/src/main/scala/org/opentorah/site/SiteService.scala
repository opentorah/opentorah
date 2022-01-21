package org.opentorah.site

import org.opentorah.docbook.DocBookConfiguration
import org.opentorah.store.{Path, Pure, Store}
import org.opentorah.tei.LinksResolver
import org.opentorah.util.{Effects, Files, Logging, Strings, Zhttp}
import org.opentorah.xml.{Caching, Element, From, Parsable, Parser, Unparser}
import net.logstash.logback.argument.{StructuredArgument, StructuredArguments}
import io.netty.handler.codec.http.HttpHeaderNames
import org.slf4j.Logger
import Zhttp.given
import zhttp.http.*
import zio.stream.ZStream
import zio.{Chunk, Duration, RIO, Task, UIO, ZIO}
import java.io.File
import java.net.URL

abstract class SiteService[S <: Site] extends Element[S]("site"), zio.ZIOAppDefault:

  // This is supposed to be set when running in Cloud Run
  private val serviceName: Option[String] = Option(System.getenv("K_SERVICE"))

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  final override type Environment = zio.ZEnv

  final def readSite(url: String): Task[S] = readSite(Files.string2url(url))

  private final def readSite(url: URL): Task[S] =
    val siteFileUrl: URL = Files.fileInDirectory(url, "site.xml")
    val result: Parser[S] = for
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl"))
      // TODO abstract over Xml?
      result: S <- parse(From.url(siteFileUrl))
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl - done"))
    yield result
    Parser.toTask(result)

  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  // TODO does this belong in the base class?
  protected def bucketName: String

  final override def run: ZIO[Environment & zio.ZIOAppArgs, Any, Any] = for
    args: Chunk[String] <- getArgs
    result: Any <- run(args)
  yield result

  private def run(args: Chunk[String]): RIO[Environment, Any] =
    if args.isEmpty then serve(urlString = None) else args.head match
      case "serveRemoteSite"         => serve(urlString = None)
      case "serveSite"               => serve(urlString = args.lift(1))
      case "buildAndPrettyPrintSite" => build(prettyPrint = true , args.drop(1))
      case "buildSite"               => build(prettyPrint = false, args.drop(1))
      case "uploadSite"              => upload(dryRun = false, args.drop(1))
      case "uploadSiteDryRun"        => upload(dryRun = true , args.drop(1))

  private def build(prettyPrint: Boolean, args: Chunk[String]): Task[Unit] =
    val directoryPath: String = args(0)
    val globalSubstitutions: Map[String, String] = args.lift(1).fold(Map.empty)(Strings.toMap)
    for
      site: S <- readSite(Files.file2url(File(directoryPath)))
      _ <- site.build(prettyPrint, globalSubstitutions)
    yield ()

  private def upload(dryRun: Boolean, args: Chunk[String]): Task[Unit] =
    val directoryPath: String = args(0)
    val serviceAccountKey: String = args(1)
    ZIO.succeedBlocking(GoogleCloudStorageSynchronizer(
      serviceAccountKey = serviceAccountKey,
      bucketName = bucketName,
      bucketPrefix = "",
      directoryPath = directoryPath + "/",
      dryRun = dryRun
    ).sync())

  // TODO RIO? URIO?
  private def serve(urlString: Option[String]): zio.URIO[Environment, Any] =
    def getParameter(name: String, defaultValue: String): String = scala.util.Properties.envOrNone(name).fold {
      info(s"No value for '$name' in the environment; using default: '$defaultValue'")
      defaultValue
    }(value =>
      info(s"Value    for '$name' in the environment: $value")
      value
    )

    val siteUrl: String = if urlString.nonEmpty then
      val result: String = urlString.get
      info(s"siteUri argument supplied: $result")
      result
    else getParameter("STORE", s"http://$bucketName/") // TODO switch to https

    val port: Int = getParameter("PORT", "8080").toInt

    warning(s"serviceName=$serviceName")

    Zhttp.start(port, routes(siteUrl))

  private def routes(siteUrl: String): RHttpApp[Environment] =
    var cachedSite: Option[S] = None
    def getSite: Task[S] = cachedSite.map(Task.succeed(_)).getOrElse(readSite(siteUrl).map(result =>
      cachedSite = Some(result)
      result
    ))

    Effects.unsafeRun(getSite)

    def reset(request: Request): Task[Response] = for
      _ <- ZIO.succeedBlocking({
        info(request, "RST")
        cachedSite = None
      })
      _ <- getSite
    yield Response(
      headers = Headers.contentType(HeaderValues.textPlain),
      data = Zhttp.textData("Site reset!")
    )

    def get(request: Request): UIO[Response] =//TODO generalize from UIO
      val pathString: String = request.url.path.toString
      val path: Seq[String] = Files.splitAndDecodeUrl(pathString)

      getSite.flatMap(site =>
        if site.isStatic(path) then Zhttp.staticFile(
          url = Files.pathUnder(Files.string2url(siteUrl), pathString),
          request = Some(request)
        ) else site.getResponse(pathString).map(siteResponse =>
          val bytes: Array[Byte] = Zhttp.textBytes(siteResponse.content)

          Response(
            headers =
              // TODO: `Content-Type`(MediaType.unsafeParse(siteResponse.mimeType), Charset.`UTF-8`)
              // TODO more headers!
              Headers.contentType(siteResponse.mimeType) ++
              Headers.contentLength(bytes.length.toLong),
            data = HttpData.fromStream(ZStream.fromChunk(Chunk.fromArray(bytes)))
          )
        )
      ).catchAll(error => ZIO.succeed(Zhttp.notFound(pathString + "\n" + error.getMessage)))

    Http.collectZIO[Request] {
      case request@Method.GET -> !! / "reset-cached-site" => reset(request)
      case request => time(request, get(request))
    }

  private def time(
    request: Request,
    response: RIO[zio.Clock, Response]
  ): RIO[zio.Clock, Response] =
    response.timed.mapAttempt((duration: Duration, response: Response) =>
      val (isWarning: Boolean, code: String) = response match
        case response: Response => response.status match
          case Status.OK           => (false, "GOT")
          case Status.NOT_FOUND    => (true , "NOT")
          case Status.NOT_MODIFIED => (false, "UNM")
          case _                   => (false, "---")

      val durationStr: String = SiteService.formatDuration(duration)
      val message: String = s"$code $durationStr ${request.url.path.toString}"

      if isWarning then
        warning(request, message)
      else
        info   (request, message)

      response
    )

  // TODO move into Logging?

  private def info   (request: Option[Request], message: String): Unit = log(     request , message, "INFO"   )
  private def info   (request:        Request , message: String): Unit = log(Some(request), message, "INFO"   )
  private def info   (                          message: String): Unit = log(None         , message, "INFO"   )
  private def notice (request:        Request , message: String): Unit = log(Some(request), message, "NOTICE" )
  private def notice (                          message: String): Unit = log(None         , message, "NOTICE" )
  private def warning(request:        Request , message: String): Unit = log(Some(request), message, "WARNING")
  private def warning(                          message: String): Unit = log(None         , message, "WARNING")

  private final def logger: Logger = Site.logger

  private def log(request: Option[Request], message: String, severity: String): Unit =
    val trace: Option[String] = request.flatMap(_.headerValue("X-Cloud-Trace-Context")).map(_.split("/")(0))

    val arguments: Seq[StructuredArgument] =
      Seq(StructuredArguments.keyValue("severity", severity)) ++
      trace.toSeq.map(trace => StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace"))

    logger.info(message, arguments*)

object SiteService:

  private def formatDuration(duration: Duration): String =
    val millis: Long = duration.toMillis
    if millis < 1000 then s"$millis ms" else
      val seconds: Float = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"

// Note: this is on the top level and not nested in the companion object,
// *and* is not the companion object since in those cases Scala compiler does not generate static main()...
object SiteServiceCommon extends SiteService[Site]:
  override def projectId: String = "???"
  override def bucketName: String = "???"

  override def contentParsable: Parsable[Site] = new Parsable[Site]:
    override def unparser: Unparser[Site] = Unparser.concat[Site](
      SiteCommon.required(_.common),
    )

    override def parser: Parser[Site] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      common: SiteCommon <- SiteCommon.required()
    yield new Site(fromUrl, common) with Pure[?]:
      override def storesPure: Seq[Store] = Seq.empty
      override def content(path: Path, extension: Option[String]): Caching.Parser[Site.Response] = ???
      override def pathShortener: Caching.Parser[Path.Shortener] = ZIO.succeed(identity)
      override def path(store: Store): Path = Seq.empty
      override protected def linkResolver(path: Path, pathShortener: Path.Shortener): LinksResolver = LinksResolver.empty
