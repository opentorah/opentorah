package org.opentorah.service

import org.opentorah.util.{Effects, Files}
import zio.http.{Body, Header, Headers, MediaType, Request, Response, Status}
import zio.ZIO
import java.time.{Instant, ZoneId, ZonedDateTime}

// TODO use zio-http middleware to handle static resources and files in a centralized manner.
object Static:
  // Inspired by https://github.com/http4s/http4s/blob/main/core/jvm/src/main/scala/org/http4s/StaticFile.scala

  def resource(
    name: String,
    request: Option[Request] = None,
    classloader: Option[ClassLoader] = None
  ): zio.Task[Response] =
    val loader: ClassLoader = classloader.getOrElse(getClass.getClassLoader)
    val normalizedName: String = name.split("/").filter(_.nonEmpty).mkString("/") // TODO Files.splitUrl
    for
      resourceOpt <- ZIO.attemptBlocking(Option(loader.getResource(normalizedName)))
      result <- resourceOpt.map(file(_, request)).getOrElse(Effects.fail(s"No such resource: $normalizedName"))
    yield result

  def file(url: java.net.URL, request: Option[Request]): zio.Task[Response] =
    for
      isDirectory: Boolean <- ZIO.attemptBlocking((url.getProtocol == "file") && java.io.File(url.getFile).isDirectory)
      _ <- Effects.check(!isDirectory, s"Is a directory: $url")

      urlConnection: java.net.URLConnection <- ZIO.attemptBlocking(url.openConnection)

      lastModifiedMilliseconds: Long <- ZIO.attemptBlocking(urlConnection.getLastModified)
      lastModified: ZonedDateTime = Instant.ofEpochSecond(lastModifiedMilliseconds / 1000).atZone(ZoneId.systemDefault)
      ifModifiedSince: Option[ZonedDateTime] = request.flatMap(_.header(Header.IfModifiedSince).map(_.value))
      expired: Boolean = ifModifiedSince.forall(_.isBefore(lastModified))

      result <- if !expired then
        for _ <- ZIO
          .attemptBlocking(urlConnection.getInputStream.close())
          .catchAll(_ => ZIO.succeed(()))
        yield Response(status = Status.NotModified)
      else
        for
          contentLength: Long <- ZIO.attemptBlocking(urlConnection.getContentLengthLong)
          inputStream: java.io.InputStream <- ZIO.attemptBlocking(urlConnection.getInputStream)
        yield
          val lastModifiedHeader: Header.LastModified = Header.LastModified(lastModified)

          val contentLengthHeader: Header =
            if contentLength >= 0
            then Header.ContentLength(contentLength)
            else Header.TransferEncoding.Chunked

          // ContentType must be supplied for files with certain extensions for the browser to correctly:
          // css: apply stylesheets (since recently)
          //  js: process scripts
          // svg: process SVG
          val contentTypeHeader: Option[Header.ContentType] = Files
            .nameAndExtension(url.getPath)
            ._2
            .flatMap(MediaType.forFileExtension)
            .map((mediaType: MediaType) => Header.ContentType(mediaType))

          Response(
            headers = Headers(Seq(
              lastModifiedHeader,
              contentLengthHeader
            ) ++
              contentTypeHeader.toSeq
            ),
            body = Body.fromStream(zio.stream.ZStream.fromInputStream(inputStream))
          )
    yield result
