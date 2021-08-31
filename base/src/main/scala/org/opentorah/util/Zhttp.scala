package org.opentorah.util

import io.netty.handler.codec.http.HttpHeaderNames
import zhttp.http.{HTTP_CHARSET, Header, HttpData, RHttpApp, Request, Response, Status}
import zhttp.service.{EventLoopGroup, Server}
import zhttp.service.server.ServerChannelFactory
import zio.blocking.Blocking
import zio.stream.ZStream
import zio.{Chunk, ZIO}

import java.io.{File, FileNotFoundException, InputStream}
import java.net.{URL, URLConnection}
import java.time.Instant

object Zhttp {

  type BResponse = ZIO[Blocking, Throwable, Option[Response[Blocking, Throwable]]]

  def textData(text: String): HttpData.CompleteData = HttpData.CompleteData(Chunk.fromArray(textBytes(text)))

  def textBytes(text: String): Array[Byte] = text.getBytes(HTTP_CHARSET)

  def queryParameter(request: Request, name: String): Option[String] = request.url.queryParams.get(name).map(_.last)

  def notFound(path: String): Response[Any, Nothing] = Response.http(
    status = Status.NOT_FOUND,
    content = Zhttp.textData(s"File Not Found: $path")
  )

  def start(
    port: Int,
    routes: RHttpApp[zio.ZEnv],
    nThreads: Int = 0
  ): zio.URIO[zio.ZEnv, zio.ExitCode] = {
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
      zio.console.putStrLn(s"Server started on port $port") // TODO use logging

      // Ensures the server doesn't die after printing
      *> ZIO.never
    )
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err")) // TODO use logging
      .exitCode
  }

  // Inspired by https://github.com/http4s/http4s/blob/main/core/jvm/src/main/scala/org/http4s/StaticFile.scala

  def staticResource(
    name: String,
    request: Option[Request] = None,
    classloader: Option[ClassLoader] = None
  ): BResponse = {
    val loader: ClassLoader = classloader.getOrElse(getClass.getClassLoader)
    val normalizedName: String = name.split("/").filter(_.nonEmpty).mkString("/") // TODO Files.splitUrl
    for {
      resource <- Effects.attempt(Option(loader.getResource(normalizedName)))
      result <- resource.fold[BResponse](ZIO.succeed(None))(staticFile(_, request))
    } yield result
  }

  def staticFile(url: URL, request: Option[Request]): BResponse = {
    for {
      isDirectory: Boolean <- Effects.attempt((url.getProtocol == "file") && new File(url.getFile).isDirectory)
      // TODO if isDirectory: ZIO.succeed(None)

      urlConnection: URLConnection <- Effects.attempt(url.openConnection)

      lastModifiedMilliseconds: Long <- Effects.attempt(urlConnection.getLastModified)
      lastModified: Instant = Instant.ofEpochSecond(lastModifiedMilliseconds / 1000)
      ifModifiedSince: Option[Instant] = request.flatMap(_.getHeaderValue(HttpHeaderNames.IF_MODIFIED_SINCE)).map(Instant.parse)
      expired: Boolean = ifModifiedSince.fold(true)(_.isBefore(lastModified))

      response <- if (!expired) {
        for {
          _ <- Effects.attempt(urlConnection.getInputStream.close()).catchAll(_ => ZIO.succeed(()))
        } yield Response.http(status = Status.NOT_MODIFIED)
      } else {
        for {
          contentLength: Long <- Effects.attempt(urlConnection.getContentLengthLong)
          inputStream: InputStream <- Effects.attempt(urlConnection.getInputStream)
        } yield Response.http(
          headers = List(
            Header(HttpHeaderNames.LAST_MODIFIED, lastModified.toString),
            if (contentLength >= 0) Header.contentLength(contentLength) else Header.transferEncodingChunked
          ) ++ nameToContentType(url.getPath).map(Header(HttpHeaderNames.CONTENT_TYPE, _)).toList,
          content = HttpData.fromStream(ZStream.fromInputStream(inputStream))
        )
      }

      result = Some(response)
    } yield result
  }.catchSome {
    case _: FileNotFoundException => ZIO.succeed(None)
  }

  private def nameToContentType(name: String): Option[String] = Files.nameAndExtension(name)._2.map {
    case "js"           => "application/javascript" // Note: without this, browser does not process scripts
    case "svg"          => "image/svg+xml"          // Note: without this, browser does not process SVG
    case "jpg" | "jpeg" => "image/jpeg"
    case _ => ""
  }
}
