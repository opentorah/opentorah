package org.opentorah.service

import org.opentorah.util.{Files, Logging}
import org.slf4j.{Logger, LoggerFactory}
import zio.http.{App, Body, Header, HttpApp, Method, Path, Request, Response, Server, Status}
import zio.{ZIO, ZLayer}
import ServiceApp.given

// TODO use zio-http middleware to:
// - auto-time (as opposed to the current manual approach with timed()) all requests (not just GETs);
// - turn failures into appropriate responses (ala orNotFound()).
// TODO use Zio logging with logstash
trait ServiceApp extends zio.ZIOAppDefault:
  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  final protected val serviceName: Option[String] = ServiceApp.getServiceName

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  protected def log: Logger = LoggerFactory.getLogger(this.getClass)

  final protected lazy val logger: GCPLogger = GCPLogger(projectId, log)

  final protected def getParameter(name: String, defaultValue: String): String =
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

  protected def run(args: zio.Chunk[String]): ZIO[Any, Throwable, Any]

  final protected def serve(app: App[Any]): ZIO[Any, Throwable, Nothing] =
    val port: Int = getParameter("PORT", 8080.toString).toInt
    logger.warning(s"serviceName=$serviceName; port=$port") // TODO more information

    // Note: To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1.
    val config: Server.Config = Server.Config.default.port(port)

    Server.serve(app).provide(
      Server.live,
      ZLayer.succeed(config)
    )

//  private def staticRoutes: HttpApp[Any, Throwable] = Http.collectZIO[Request] {
//    case request @ Method.GET -> !! / path if isStaticResource(path) =>
//      ServiceApp.orNotFound(path, ServiceApp.staticResource("/" + path, Some(request)))
//
//
//      val pathString: String = request.url.path.toString
//      val path: Seq[String] = Files.splitAndDecodeUrl(pathString)
//
//      ServiceApp.orNotFound(pathString, getSite.flatMap(site =>
//        if site.isStatic(path) then ServiceApp.staticFile(
//          url = Files.pathUnder(Files.string2url(siteUrl), pathString),
//          request = Some(request)
//        )
//
//      ServiceApp.orNotFound(pathString,
//        if isStatic(path) then ServiceApp.staticFile(
//          url = Files.pathUnder(Files.string2url(siteUrl), pathString),
//          request = Some(request)
//        )
//  }
//
//  protected def isStaticResource(path: String): Boolean = false
//  protected def isStaticFile    (path: String): Boolean = false

  final protected def timed[R](
    get: Request => ZIO[R, Throwable, Response]
  ): Request => ZIO[R, Throwable, Response] = (request: Request) =>
    val response: ZIO[R, Throwable, Response] = get(request)

    response.timed.mapAttempt((duration: zio.Duration, response: Response) =>
      val (isWarning: Boolean, code: String) = response match
        case response: Response => response.status match
        case Status.Ok          => (false, "GOT")
        case Status.NotFound    => (true , "NOT")
        case Status.NotModified => (false, "UNM")
        case _                  => (false, "---")

      val durationStr: String = ServiceApp.formatDuration(duration)
      val message: String = s"$code $durationStr ${request.url.path.toString}"

      if isWarning then
        logger.warning(request, message)
      else
        logger.info   (request, message)

      response
    )

object ServiceApp:
  // This is set when running in Cloud Run (or KNative in general?)
  def getServiceName: Option[String] = Option(System.getenv("K_SERVICE"))

  given CanEqual[Method, Method] = CanEqual.derived
  given CanEqual[Path  , Path  ] = CanEqual.derived
  given CanEqual[Status, Status] = CanEqual.derived

  def queryParameter(request: Request, name: String): Option[String] = request.url.queryParams.get(name).map(_.last)

  private def formatDuration(duration: zio.Duration): String =
    val millis: Long = duration.toMillis
    if millis < 1000 then s"$millis ms" else
      val seconds: Float = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"

  def orNotFound[R](path: String, zio: ZIO[R, Throwable, Response]): ZIO[R, Nothing, Response] =
    zio.catchAll((error: Throwable) => ZIO.succeed(Response(
      status = Status.NotFound,
      body = Body.fromString(s"File Not Found: $path \n ${error.getMessage}")
    )))
