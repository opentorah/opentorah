package org.opentorah.collector

import cats.effect.ExitCode
import org.http4s.implicits._ // orNotFound
import org.http4s.server.blaze.BlazeServerBuilder
import zio.{App, RIO, ZEnv, ZIO}
import zio.interop.catz._

object Service extends App {

  /*
    Headers:
      from store.alter-rebbe.org (GitHub):

    Server: GitHub.com
    Content-Type: text/html; charset=utf-8
    Last-Modified: Sun, 19 Jul 2020 23:05:58 GMT
    ETag: "5f14d1d6-128b"
    Access-Control-Allow-Origin: *
    Expires: Mon, 20 Jul 2020 05:40:30 GMT
    Cache-Control: max-age=600
    X-Proxy-Cache: MISS
    X-GitHub-Request-Id: 66C8:4BE6:21250:3B3D9:5F152BF6
    Content-Length: 4747
    Accept-Ranges: bytes
    Date: Mon, 20 Jul 2020 05:32:11 GMT
    Via: 1.1 varnish
    Age: 100
    Connection: keep-alive
    X-Served-By: cache-bos4643-BOS
    X-Cache: HIT
    X-Cache-Hits: 1
    X-Timer: S1595223131.124794,VS0,VE0
    Vary: Accept-Encoding
    X-Fastly-Request-ID: fe71b3e148b79b081959e3e7c11a00b2e3900cbb

      from localhost:
    Last-Modified: Sun, 19 Jul 2020 23:05:58 GMT
    Content-Type: text/html
    Date: Mon, 20 Jul 2020 05:38:02 GMT
    Content-Length: 4747

      from Google Cloud Run:
    last-modified: Sun, 19 Jul 2020 23:05:58 GMT
    content-type: text/html
    date: Mon, 20 Jul 2020 05:47:03 GMT
    server: Google Frontend
    content-length: 4747
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, zio.ExitCode] = {
    val serviceConfiguration: ServiceConfiguration = ServiceConfiguration.load(args)

    // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
    val server: RIO[ServiceEnvironment, Unit] = ZIO.runtime[ServiceEnvironment].flatMap { implicit rts =>
      BlazeServerBuilder[ServiceTask](executionContext = scala.concurrent.ExecutionContext.global)
        .bindHttp(host = "0.0.0.0", port = serviceConfiguration.port)
        .withWebSockets(false)
        .withHttpApp(new Api(serviceConfiguration.otherHost).routes.orNotFound)
        .serve
        .compile[ServiceTask, ServiceTask, ExitCode]
        .drain
    }

    server
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err"))
      .exitCode
  }
}
