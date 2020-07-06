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
import java.net.URL
import org.http4s.{Header, Headers, HttpDate, MediaType, Request, Response, TransferCoding}
import org.http4s.Status.NotModified
import org.http4s.headers._
import org.opentorah.util.Files

object MyStaticFile {
  val DefaultBufferSize = 10240

  def fromURL[F[_]](url: URL, blocker: Blocker, req: Option[Request[F]] = None)(implicit
                                                                                F: Sync[F],
                                                                                cs: ContextShift[F]): OptionT[F, Response[F]] = {
    val fileUrl = url.getFile
    val file = new File(fileUrl)
    OptionT.apply(F.delay {
      if (file.isDirectory)
        None
      else {
        val urlConn = url.openConnection
        val lastmod = HttpDate.fromEpochSecond(urlConn.getLastModified / 1000).toOption
        val ifModifiedSince = req.flatMap(_.headers.get(`If-Modified-Since`))
        val expired = (ifModifiedSince, lastmod).mapN(_.date < _).getOrElse(true)

        if (expired) {
          val lastModHeader: List[Header] = lastmod.map(`Last-Modified`(_)).toList
          val contentType = Files.nameAndExtension(url.getPath)._2
            .flatMap(extension => MediaType.forExtension(extension).map(`Content-Type`(_))).toList
          val len = urlConn.getContentLengthLong
          val lenHeader =
            if (len >= 0) `Content-Length`.unsafeFromLong(len)
            else `Transfer-Encoding`(TransferCoding.chunked)
          val headers = Headers(lenHeader :: lastModHeader ::: contentType)

          fromStream(F.delay(url.openStream), headers, blocker)
        } else {
          urlConn.getInputStream.close()
          Some(Response(NotModified))
        }
      }
    })
  }

  private def fromStream[F[_]](inputStream: F[InputStream], headers: Headers, blocker: Blocker)(implicit
                                                            F: Sync[F],
                                                            cs: ContextShift[F]): Option[Response[F]] = {
    Some(
      Response(
        headers = headers,
        body = readInputStream[F](F.suspend(inputStream), DefaultBufferSize, blocker)
      ))
  }
}
