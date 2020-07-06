package org.opentorah.collector

import cats.effect.ExitCode
import org.http4s.implicits._ // orNotFound
import org.http4s.server.blaze.BlazeServerBuilder
import zio.{App, RIO, ZEnv, ZIO}
import zio.interop.catz._

object Service extends App {

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
