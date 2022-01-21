package org.opentorah.service

import org.opentorah.util.{Effects, Files, Logging}
import org.slf4j.{Logger, LoggerFactory}
import zhttp.http.{!!, /, Headers, Http, HttpApp, HttpData, Method, Path, Request, Response, Status, *} // TODO remove *
import zhttp.service.Server
import zio.ZIO
import java.time.Instant
import ServiceApp.given

// TODO use ZHTTP middleware to:
// - auto-time (as opposed to the current manual approach with timed()) all requests (not just GETs);
// - turn failures into appropriate responses (ala orNotFound());
// - handle static resources and files in a centralized manner.
trait ServiceApp extends zio.ZIOAppDefault:
  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  protected def projectId: String

  // This is set when running in Cloud Run (or KNative in general?)
  final protected def serviceName: Option[String] = Option(System.getenv("K_SERVICE"))

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  final protected lazy val logger: GCPLogger = GCPLogger(projectId, log)

  protected def log: Logger = LoggerFactory.getLogger(this.getClass)

  final protected def getParameter(name: String, defaultValue: String): String =
    def result(value: String, message: String): String =
      logger.info(message)
      value

    scala.util.Properties.envOrNone(name).map((value: String) =>
      result(value       , s"Value    for '$name' in the environment; using it     : '$value'")
    ).getOrElse(
      result(defaultValue, s"No value for '$name' in the environment; using default: '$defaultValue'")
    )

  final override def run: ZIO[Environment & zio.ZIOAppArgs, Any, Any] = for
    args: zio.Chunk[String] <- getArgs
    result: Any <- run(args)
  yield result

  protected def run(args: zio.Chunk[String]): ZIO[Any, Throwable, Any]

  final protected def serve(
    routes: HttpApp[zio.Clock, Throwable],
    nThreads: Int = 0
  ): ZIO[Any, Throwable, Nothing] =
    val port: Int = getParameter("PORT", portDefault.toString).toInt

    logger.warning(s"serviceName=$serviceName") // TODO more information

    Server(routes)
      // Note: To be accessible when running in a docker container the server
      // must bind to all IPs, not just 127.0.0.1;
      // with http4s, I had to supply a "host" string "0.0.0.0",
      // but with zhttp there seems to be no way to do it - and no need :)
      .withPort(port)
      .make
      // Ensures the server doesn't die after printing
      .flatMap((start: Server.Start) => zio.Console.printLine(s"Server started on port ${start.port}") *> ZIO.never) // TODO use logger
      .tapError((error: Throwable)   => zio.Console.printLine(s"Execution failed with: $error")) // TODO use logger
      .provideSomeLayer(zio.Clock.live)
      .provideSomeLayer(zio.Console.live)
      .provideSomeLayer(
        zio.Scope.default ++
        zhttp.service.EventLoopGroup.auto(nThreads) ++
        zhttp.service.server.ServerChannelFactory.auto
      )

  protected def portDefault: Int = 8080

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

  protected def isStaticResource(path: String): Boolean = false
  protected def isStaticFile    (path: String): Boolean = false

  final protected def timed[R](
    get: Request => ZIO[R, Throwable, Response]
  ): Request => ZIO[R & zio.Clock, Throwable, Response] = (request: Request) =>
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
      data = HttpData.fromString(s"File Not Found: $path \n ${error.getMessage}")
    )))

  // Inspired by https://github.com/http4s/http4s/blob/main/core/jvm/src/main/scala/org/http4s/StaticFile.scala

  def staticResource(
    name: String,
    request: Option[Request] = None,
    classloader: Option[ClassLoader] = None
  ): zio.Task[Response] =
    val loader: ClassLoader = classloader.getOrElse(getClass.getClassLoader)
    val normalizedName: String = name.split("/").filter(_.nonEmpty).mkString("/") // TODO Files.splitUrl
    for
      resourceOpt <- ZIO.attemptBlocking(Option(loader.getResource(normalizedName)))
      result <- resourceOpt.map(staticFile(_, request)).getOrElse(Effects.fail(s"No such resource: $normalizedName"))
    yield result

  def staticFile(url: java.net.URL, request: Option[Request]): zio.Task[Response] =
    for
      isDirectory: Boolean <- ZIO.attemptBlocking((url.getProtocol == "file") && java.io.File(url.getFile).isDirectory)
      _ <- Effects.check(!isDirectory, s"Is a directory: $url")

      urlConnection: java.net.URLConnection <- ZIO.attemptBlocking(url.openConnection)

      lastModifiedMilliseconds: Long <- ZIO.attemptBlocking(urlConnection.getLastModified)
      lastModified: Instant = Instant.ofEpochSecond(lastModifiedMilliseconds / 1000)
      ifModifiedSince: Option[String] = request.flatMap(_.headerValue(io.netty.handler.codec.http.HttpHeaderNames.IF_MODIFIED_SINCE))
      expired: Boolean = ifModifiedSince.forall((ifModifiedSince: String) => Instant.parse(ifModifiedSince).isBefore(lastModified))

      result <- if !expired then
        for _ <- ZIO.attemptBlocking(urlConnection.getInputStream.close()).catchAll(_ => ZIO.succeed(()))
        yield Response(status = Status.NotModified)
      else
        for
          contentLength: Long <- ZIO.attemptBlocking(urlConnection.getContentLengthLong)
          inputStream: java.io.InputStream <- ZIO.attemptBlocking(urlConnection.getInputStream)
        yield Response(
          headers =
            Headers.lastModified(lastModified.toString) ++
              Files.nameAndExtension(url.getPath)._2
                .flatMap(name2contentType.get)
                .map(Headers.contentType)
                .getOrElse(Headers.empty) ++
              (
                if contentLength >= 0
                then Headers.contentLength(contentLength)
                else Headers.transferEncoding(zhttp.http.HeaderValues.chunked)
              ),
          data = HttpData.fromStream(zio.stream.ZStream.fromInputStream(inputStream))
        )
    yield result

  private val name2contentType: Map[String, String] = Map(
    "js"   -> "application/javascript", // Note: without this, browser does not process scripts
    "svg"  -> "image/svg+xml",          // Note: without this, browser does not process SVG
    "jpg"  -> "image/jpeg",
    "jpeg" -> "image/jpeg"
  )
