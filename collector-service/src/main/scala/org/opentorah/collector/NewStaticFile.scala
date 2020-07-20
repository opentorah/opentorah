/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package org.opentorah.collector

import cats.data.OptionT
import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import fs2.io._
import java.io._
import java.net.{URL, URLConnection}
import org.http4s.{Header, Headers, HttpDate, MediaType, Request, Response, TransferCoding}
import org.http4s.Status.NotModified
import org.http4s.headers._
import org.opentorah.util.Files
import scala.util.Try

object NewStaticFile {
  val DefaultBufferSize = 10240

  def fromURL[F[_]](url: URL, blocker: Blocker, req: Option[Request[F]] = None)(implicit
                                                                                F: Sync[F],
                                                                                cs: ContextShift[F]): OptionT[F, Response[F]] = {
    val file = new File(url.getFile)
    OptionT.apply(F.delay[Option[Response[F]]] {
      if (file.isDirectory) None else {
        val urlConn: URLConnection = url.openConnection
        val lastmod: Option[HttpDate] = HttpDate.fromEpochSecond(urlConn.getLastModified / 1000).toOption
        val ifModifiedSince: Option[`If-Modified-Since`] = req.flatMap(_.headers.get(`If-Modified-Since`))
        val expired: Boolean = (ifModifiedSince, lastmod).mapN(_.date < _).getOrElse(true)

        if (!expired) {
          urlConn.getInputStream.close()
          Some(Response(NotModified))
        } else {

          Try(url.openStream()).fold(
            fa = {
              case _: FileNotFoundException => None
            },
            fb = { inputStream =>
              Some(
                Response(
                  headers = headers(lastmod, url, urlConn.getContentLengthLong),
                  body = readInputStream[F](F.delay(inputStream), DefaultBufferSize, blocker)
                ))
            }
          )
        }
      }
    })
  }

  private def headers(
    lastmod: Option[HttpDate],
    url: URL,
    len: Long
  ): Headers = {
    val lastModHeader: List[Header] = lastmod.map(`Last-Modified`(_)).toList
    val contentType = Files.nameAndExtension(url.getPath)._2
      .flatMap(extension => MediaType.forExtension(extension).map(`Content-Type`(_))).toList
    val lenHeader =
      if (len >= 0) `Content-Length`.unsafeFromLong(len)
      else `Transfer-Encoding`(TransferCoding.chunked)
    Headers(lenHeader :: lastModHeader ::: contentType)
  }
}
