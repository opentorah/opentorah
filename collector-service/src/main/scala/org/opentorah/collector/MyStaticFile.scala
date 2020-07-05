/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */
// TODO url.openStream() throws FileNotFoundException that kills the fiber - and the Cloud Run instance.
// 1) open an issue; 2) find a temporary work-around.
package org.opentorah.collector

import cats.data.OptionT
import cats.effect.{Blocker, ContextShift, IO, Sync}
import cats.implicits._
import fs2.io._
import java.io._
import java.net.URL

import org.http4s.{Header, Headers, HttpDate, MediaType, Request, Response, TransferCoding}
import org.http4s.Status.NotModified
import org.http4s.headers._

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
          val contentType = nameToContentType(url.getPath).toList
          val len = urlConn.getContentLengthLong
          val lenHeader =
            if (len >= 0) `Content-Length`.unsafeFromLong(len)
            else `Transfer-Encoding`(TransferCoding.chunked)
          val headers = Headers(lenHeader :: lastModHeader ::: contentType)

          Some(
            Response(
              headers = headers,
              body = readInputStream[F](F.delay(url.openStream), DefaultBufferSize, blocker)
            ))
        } else {
          urlConn.getInputStream.close()
          Some(Response(NotModified))
        }
      }
    })
  }

  private def nameToContentType(name: String): Option[`Content-Type`] =
    name.lastIndexOf('.') match {
      case -1 => None
      case i => MediaType.forExtension(name.substring(i + 1)).map(`Content-Type`(_))
    }
}
