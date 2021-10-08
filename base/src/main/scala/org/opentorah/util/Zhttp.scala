package org.opentorah.util

import io.netty.handler.codec.http.HttpHeaderNames
import zhttp.http.{HTTP_CHARSET, Header, HttpData, Method, Path, RHttpApp, Request, Response, ResponseM, Status}
import zhttp.service.{EventLoopGroup, Server}
import zhttp.service.server.ServerChannelFactory
import zio.blocking.Blocking
import zio.stream.ZStream
import zio.{Chunk, ZIO}

import java.io.{File, FileNotFoundException, InputStream}
import java.net.{URL, URLConnection}
import java.time.Instant

object Zhttp:

  given CanEqual[Method, Method] = CanEqual.derived
  given CanEqual[Path, Path] = CanEqual.derived
  given CanEqual[Status, Status] = CanEqual.derived

  def textData(text: String): HttpData.CompleteData = HttpData.CompleteData(Chunk.fromArray(textBytes(text)))

  def textBytes(text: String): Array[Byte] = text.getBytes(HTTP_CHARSET)

  def queryParameter(request: Request, name: String): Option[String] = request.url.queryParams.get(name).map(_.last)

  def notFound(pathAndMessage: String): Response[Any, Nothing] = Response.http(
    status = Status.NOT_FOUND,
    content = Zhttp.textData(s"File Not Found: $pathAndMessage")
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
      // but with zhhtp there seems to be no way to do it - and no need :)
      Server.port(port) ++
      //Server.paranoidLeakDetection ++ // Paranoid leak detection (affects performance)
      Server.app(routes)

    server.make.use(_ =>
      // Waiting for the server to start
      // TODO use logging
      zio.console.putStrLn(s"Server started on port $port") *> ZIO.never
      // Ensures the server doesn't die after printing
    )
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err")) // TODO use logging
      .exitCode
  end start

  // Inspired by https://github.com/http4s/http4s/blob/main/core/jvm/src/main/scala/org/http4s/StaticFile.scala

  def staticResource(
    name: String,
    request: Option[Request] = None,
    classloader: Option[ClassLoader] = None
  ): ResponseM[Blocking, Throwable] =
    val loader: ClassLoader = classloader.getOrElse(getClass.getClassLoader)
    val normalizedName: String = name.split("/").filter(_.nonEmpty).mkString("/") // TODO Files.splitUrl
    for
      resourceOpt <- Effects.attempt(Option(loader.getResource(normalizedName)))
      result <- if resourceOpt.isEmpty then Effects.fail(s"No such resource: $normalizedName") else staticFile(resourceOpt.get, request)
    yield result

  def staticFile(url: URL, request: Option[Request]): ResponseM[Blocking, Throwable] =
    for
      isDirectory: Boolean <- Effects.attempt((url.getProtocol == "file") && File(url.getFile).isDirectory)
      _ <- Effects.check(!isDirectory, s"Is a directory: $url")

      urlConnection: URLConnection <- Effects.attempt(url.openConnection)

      lastModifiedMilliseconds: Long <- Effects.attempt(urlConnection.getLastModified)
      lastModified: Instant = Instant.ofEpochSecond(lastModifiedMilliseconds / 1000)
      ifModifiedSince: Option[Instant] = request.flatMap(_.getHeaderValue(HttpHeaderNames.IF_MODIFIED_SINCE)).map(Instant.parse)
      expired: Boolean = ifModifiedSince.fold(true)(_.isBefore(lastModified))

      result <- if !expired then
        for
          _ <- Effects.attempt(urlConnection.getInputStream.close()).catchAll(_ => ZIO.succeed(()))
        yield Response.http(status = Status.NOT_MODIFIED)
      else
        for
          contentLength: Long <- Effects.attempt(urlConnection.getContentLengthLong)
          inputStream: InputStream <- Effects.attempt(urlConnection.getInputStream)
        yield Response.http(
          headers = List(
            Header.custom(HttpHeaderNames.LAST_MODIFIED.toString, lastModified.toString),
            if contentLength >= 0 then Header.contentLength(contentLength) else Header.transferEncodingChunked
          ) ++ nameToContentType(url.getPath).map(Header.custom(HttpHeaderNames.CONTENT_TYPE.toString, _)).toList,
          content = HttpData.fromStream(ZStream.fromInputStream(inputStream))
        )
    yield result

  private def nameToContentType(name: String): Option[String] = Files.nameAndExtension(name)._2.map {
    case "js"   => "application/javascript" // Note: without this, browser does not process scripts
    case "svg"  => "image/svg+xml"          // Note: without this, browser does not process SVG
    case "jpg"  => "image/jpeg"
    case "jpeg" => "image/jpeg"
    case _      => ""
  }
