package org.opentorah.util

import io.netty.handler.codec.http.HttpHeaderNames
import zhttp.http.{Headers, HeaderValues, HTTP_CHARSET, HttpData, Method, Path, Request, Response, RHttpApp, Status}
import zhttp.service.{EventLoopGroup, Server}
import zhttp.service.server.ServerChannelFactory
import zio.stream.ZStream
import zio.{Chunk, Task, ZIO}
import java.io.{File, InputStream}
import java.net.{URL, URLConnection}
import java.time.Instant

object Zhttp:

  given CanEqual[Method, Method] = CanEqual.derived
  given CanEqual[Path, Path] = CanEqual.derived
  given CanEqual[Status, Status] = CanEqual.derived

  def textData(text: String): HttpData = HttpData.fromChunk(Chunk.fromArray(textBytes(text)))

  def textBytes(text: String): Array[Byte] = text.getBytes(HTTP_CHARSET)

  def queryParameter(request: Request, name: String): Option[String] = request.url.queryParams.get(name).map(_.last)

  def notFound(pathAndMessage: String): Response = Response(
    status = Status.NOT_FOUND,
    data = Zhttp.textData(s"File Not Found: $pathAndMessage")
  )

  def start(
    port: Int,
    routes: RHttpApp[zio.ZEnv],
    nThreads: Int = 0
  ): zio.URIO[zio.ZEnv, zio.ExitCode] =
    val server =
    // Note: To be accessible when running in a docker container the server
    // must bind to all IPs, not just 127.0.0.1;
    // with http4s, I had to supply a "host" string "0.0.0.0",
    // but with zhttp there seems to be no way to do it - and no need :)
      Server.port(port) ++
      //Server.paranoidLeakDetection ++ // Paranoid leak detection (affects performance)
      Server.app(routes)

    // TODO better with       .flatMap(start => ZManaged.succeed(println(s"Server started on port: ${start.port}")))?
    server.make.use(_ =>
      // Waiting for the server to start
      // TODO use logging
      zio.Console.printLine(s"Server started on port $port") *> ZIO.never
      // Ensures the server doesn't die after printing
    )
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))
      .mapError(err => zio.Console.printLine(s"Execution failed with: $err")) // TODO use logging
      .exitCode
  end start

  // Inspired by https://github.com/http4s/http4s/blob/main/core/jvm/src/main/scala/org/http4s/StaticFile.scala

  def staticResource(
    name: String,
    request: Option[Request] = None,
    classloader: Option[ClassLoader] = None
  ): Task[Response] =
    val loader: ClassLoader = classloader.getOrElse(getClass.getClassLoader)
    val normalizedName: String = name.split("/").filter(_.nonEmpty).mkString("/") // TODO Files.splitUrl
    for
      resourceOpt <- ZIO.attemptBlocking(Option(loader.getResource(normalizedName)))
      result <- if resourceOpt.isEmpty then Effects.fail(s"No such resource: $normalizedName") else staticFile(resourceOpt.get, request)
    yield result

  def staticFile(url: URL, request: Option[Request]): Task[Response] =
    for
      isDirectory: Boolean <- ZIO.attemptBlocking((url.getProtocol == "file") && File(url.getFile).isDirectory)
      _ <- Effects.check(!isDirectory, s"Is a directory: $url")

      urlConnection: URLConnection <- ZIO.attemptBlocking(url.openConnection)

      lastModifiedMilliseconds: Long <- ZIO.attemptBlocking(urlConnection.getLastModified)
      lastModified: Instant = Instant.ofEpochSecond(lastModifiedMilliseconds / 1000)
      ifModifiedSince: Option[Instant] = request.flatMap(_.headerValue(HttpHeaderNames.IF_MODIFIED_SINCE)).map(Instant.parse)
      expired: Boolean = ifModifiedSince.fold(true)(_.isBefore(lastModified))

      result <- if !expired then
        for
          _ <- ZIO.attemptBlocking(urlConnection.getInputStream.close()).catchAll(_ => ZIO.succeed(()))
        yield Response(status = Status.NOT_MODIFIED)
      else
        for
          contentLength: Long <- ZIO.attemptBlocking(urlConnection.getContentLengthLong)
          inputStream: InputStream <- ZIO.attemptBlocking(urlConnection.getInputStream)
        yield Response(
          headers =
            Headers.lastModified(lastModified.toString) ++
            (if contentLength >= 0 then Headers.contentLength(contentLength) else Headers.transferEncoding(HeaderValues.chunked)) ++
            nameToContentType(url.getPath).fold(Headers.empty)(Headers.contentType(_)),
          data = HttpData.fromStream(ZStream.fromInputStream(inputStream))
        )
    yield result

  private def nameToContentType(name: String): Option[String] = Files.nameAndExtension(name)._2.map {
    case "js"   => "application/javascript" // Note: without this, browser does not process scripts
    case "svg"  => "image/svg+xml"          // Note: without this, browser does not process SVG
    case "jpg"  => "image/jpeg"
    case "jpeg" => "image/jpeg"
    case _      => ""
  }
