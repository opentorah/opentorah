package org.opentorah.collector

import org.http4s.{HttpRoutes, Uri}
import org.http4s.dsl.Http4sDsl
import zio.interop.catz._
import Service.ServiceTask

object ServiceRoutes {

  /* Headres:

    store.alter-rebbe.org (GitHub)          localhost:                      Google Cloud Run:
    ------------------------------          ----------                      -----------------
    Date: ...                               Date: ...                       date:
    Last-Modified: ...                      Last-Modified: ...              last-modified:
    Content-Type: text/html; charset=utf-8  Content-Type: text/html         content-type: text/html
    Content-Length: 4747                    Content-Length: 4747            content-length: 4747
    Server: GitHub.com                                                      server: Google Frontend

    ETag: "5f14d1d6-128b"
    Expires: ...
    Access-Control-Allow-Origin: *
    Cache-Control: max-age=600
    Age: 100
    Accept-Ranges: bytes
    Vary: Accept-Encoding
    Via: 1.1 varnish
    Connection: keep-alive
    X-Served-By: cache-bos4643-BOS
    X-Timer: S1595223131.124794,VS0,VE0
    X-Fastly-Request-ID: fe71b3e148b79b081959e3e7c11a00b2e3900cbb
    X-Proxy-Cache: MISS
    X-Cache: HIT
    X-Cache-Hits: 1
    X-GitHub-Request-Id: 66C8:4BE6:21250:3B3D9:5F152BF6
   */

  def routes(store: Uri): HttpRoutes[ServiceTask] = {
    val dsl: Http4sDsl[ServiceTask] = Http4sDsl[ServiceTask]
    import dsl._

    HttpRoutes.of[ServiceTask] {
      //      case GET -> Root / "hello" =>
      //        Ok("hello!")

      case request@GET -> _ =>
        val uri: Uri = store.resolve(relativize(addIndex(request.uri)))
        Service.fromUri(uri, request)
    }
  }

  private def addIndex(uri: Uri): Uri =
    if (uri.path.endsWith("/")) uri.copy(path = uri.path + "index.html") else uri

  private def relativize(uri: Uri): Uri =
    if (uri.path.startsWith("/")) uri.copy(path = uri.path.substring(1)) else uri
}
