package org.opentorah.site

import org.opentorah.gcp.{GCP, GCPLogger, GoogleCloudStorageSynchronizer}
import org.opentorah.service.Static
import org.opentorah.util.{Effects, Files, Logging}
import org.opentorah.xml.{ElementTo, From, Parser}
import zio.http.{Body, Charsets, Header, Headers, MediaType, Method, Path, Request, Response, Route, Routes,
  Server, Status, handler, trailing}
import zio.{Chunk, Task, ZIO, ZLayer}
import java.io.File
import java.net.URL

// TODO merge into the Collector?
// TODO use zio-http middleware to:
// - auto-time (as opposed to the current manual approach with timed()) all requests (not just GETs);
// - turn failures into appropriate responses (ala orNotFound()).
// TODO use Zio logging with logstash
abstract class SiteService[S <: Site] extends ElementTo[S]("site"), zio.ZIOAppDefault:
  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  private val serviceName: Option[String] = GCP.getServiceName

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  private lazy val logger: GCPLogger = GCPLogger(projectId, Site.logger)

  private def getParameter(name: String, defaultValue: String): String =
    def result(value: String, message: String): String =
      logger.info(message)
      value

    scala.util.Properties.envOrNone(name).map((value: String) =>
      result(value, s"Value    for '$name' in the environment; using it     : '$value'")
    ).getOrElse(
      result(defaultValue, s"No value for '$name' in the environment; using default: '$defaultValue'")
    )

  final override def run: ZIO[Environment & zio.ZIOAppArgs, Any, Any] = for
    args: zio.Chunk[String] <- getArgs
    result: Any <- run(args)
  yield result

  private def run(args: Chunk[String]): ZIO[Any, Throwable, Any] =
    if args.isEmpty then serve(urlString = None) else args.head match
      case "serveRemoteSite" => serve(urlString = None)
      case "serveSite" => serve(urlString = args.lift(1))
      case "buildAndPrettyPrintSite" => build(prettyPrint = true, args.drop(1))
      case "buildSite" => build(prettyPrint = false, args.drop(1))
      case "uploadSite" => upload(dryRun = false, args.drop(1))
      case "uploadSiteDryRun" => upload(dryRun = true, args.drop(1))

  private def build(prettyPrint: Boolean, args: Chunk[String]): Task[Unit] =
    val directoryPath: String = args(0)
    for
      site: S <- readSite(Files.file2url(File(directoryPath)))
      _ <- site.build(prettyPrint)
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

  private def serve(urlString: Option[String]): ZIO[Any, Throwable, Any] =
    val siteUrl: String = if urlString.nonEmpty then
      val result: String = urlString.get
      logger.info(s"siteUri argument supplied: $result")
      result
    else getParameter("STORE", s"http://$bucketName/") // TODO switch to https
    
    val port: Int = getParameter("PORT", 8080.toString).toInt
    logger.warning(s"serviceName=$serviceName; port=$port") // TODO more information

    // Note: To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1.
    val config: Server.Config = Server.Config.default.port(port)

    Server.serve(routes(siteUrl).sandbox).provide(
      Server.live,
      ZLayer.succeed(config)
    )
    
  private final  def timed[R](
    get: Request => ZIO[R, Throwable, Response]
  ): Request => ZIO[R, Throwable, Response] = (request: Request) =>
    val response: ZIO[R, Throwable, Response] = get(request)

    response.timed.mapAttempt((duration: zio.Duration, response: Response) =>
      given CanEqual[Status, Status] = CanEqual.derived
      val (isWarning: Boolean, code: String) = response match
        case response: Response => response.status match
          case Status.Ok => (false, "GOT")
          case Status.NotFound => (true, "NOT")
          case Status.NotModified => (false, "UNM")
          case _ => (false, "---")

      val durationStr: String = SiteService.formatDuration(duration)
      val message: String = s"$code $durationStr ${request.url.path.toString}"

      if isWarning then
        logger.warning(request, message)
      else
        logger.info(request, message)

      response
    )
  
  private def routes(siteUrl: String): Routes[Any, Throwable] =
    var cachedSite: Option[S] = None
    def getSite: Task[S] = cachedSite.map(ZIO.succeed(_)).getOrElse(readSite(siteUrl).map(result =>
      cachedSite = Some(result)
      result
    ))

    Effects.unsafeRun(getSite)

    def reset(request: Request): Task[Response] = for
      _ <- ZIO.succeedBlocking({
        logger.info(request, "RST")
        cachedSite = None
      })
      _ <- getSite
    yield Response.text("Site reset!")

    def get(request: Request): ZIO[Any, Nothing, Response] =
      val pathString: String = request.path.toString
      val path: Seq[String] = Files.splitAndDecodeUrl(pathString)
     
      SiteService.orNotFound(pathString, getSite.flatMap(site =>
        if site.isStatic(path) then Static.file(
          url = Files.pathUnder(Files.string2url(siteUrl), pathString),
          request = Some(request)
        ) else site.getResponse(pathString).map(siteResponse =>
          val bytes: Array[Byte] = siteResponse.content.getBytes(Charsets.Http)

          Response(
            headers = Headers(
              // TODO: `Content-Type`(MediaType.unsafeParse(siteResponse.mimeType), Charset.`UTF-8`)
              // TODO more headers!
              Header.ContentType(MediaType.parseCustomMediaType(siteResponse.mimeType).get),
              Header.ContentLength(bytes.length.toLong)
            ),
            body = Body.fromArray(bytes)
          )
        )
      ))

    def static(request: Request): Task[Response] = Static.file(
      url = Files.pathUnder(Files.string2url(siteUrl), request.path.toString),
      request = Some(request)
    )
    
    //common.getHtml.favicon.toSet
    Routes.fromIterable(for staticDirectory <- Seq("assets", "css", "js", "sass") yield
      Method.GET / staticDirectory / trailing -> handler { (_: Path, request: Request) => static(request) }
    ) ++
    Routes.fromIterable(for staticFile <- Seq("robots.txt") ++ Seq() /* TODO site.common.getHtml.favicon.toSeq - and remove the if site.isStatic(path) block */ yield
      Method.GET / staticFile -> handler { (request: Request) => static(request) }
    ) ++
    Routes(
      Method.GET / "reset-cached-site" -> handler { (request: Request) => reset(request) },
      Method.GET / trailing -> handler { (_: Path, request: Request) => timed(get)(request) }
    )

  // TODO does this belong in the base class?
  protected def bucketName: String

  final def readSite(url: String): Task[S] = readSite(Files.string2url(url))

  private def readSite(url: URL): Task[S] =
    val siteFileUrl: URL = Files.fileInDirectory(url, "site.xml")
    val result: Parser[S] = for
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl"))
      // TODO abstract over Xml?
      result: S <- parse(From.url(siteFileUrl))
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl - done"))
    yield result
    Parser.toTask(result)
    
object SiteService:
  private def formatDuration(duration: zio.Duration): String =
    val millis: Long = duration.toMillis
    if millis < 1000 then s"$millis ms" else
      val seconds: Float = Math.round(millis.toFloat / 100).toFloat / 10
      s"$seconds s"

  private def orNotFound[R](path: String, zio: ZIO[R, Throwable, Response]): ZIO[R, Nothing, Response] =
    zio.catchAll((error: Throwable) => ZIO.succeed(Response(
      status = Status.NotFound,
      body = Body.fromString(s"File Not Found: $path \n ${error.getMessage}")
    )))
