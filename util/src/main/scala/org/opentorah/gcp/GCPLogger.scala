package org.opentorah.gcp

import net.logstash.logback.argument.{StructuredArgument, StructuredArguments}
import zio.http.Request

final class GCPLogger(projectId: String, logger: org.slf4j.Logger):
  def info   (request: Option[Request], message: String): Unit = log    (     request , message, "INFO"   )
  def info   (request:        Request , message: String): Unit = info   (Some(request), message)
  def info   (                          message: String): Unit = info   (None         , message)
  def notice (request: Option[Request], message: String): Unit = log    (     request , message, "NOTICE" )
  def notice (request:        Request , message: String): Unit = notice (Some(request), message)
  def notice (                          message: String): Unit = notice (None         , message)
  def warning(request: Option[Request], message: String): Unit = log    (      request, message, "WARNING")
  def warning(request:        Request , message: String): Unit = warning(Some(request), message)
  def warning(                          message: String): Unit = warning(None         , message)

  private def log(request: Option[Request], message: String, severity: String): Unit =
    val arguments: Seq[StructuredArgument] =
      Seq(
        StructuredArguments.keyValue("severity", severity)
      ) ++ request.flatMap(_.headers.get("X-Cloud-Trace-Context")).map(_.split("/")(0)).toSeq.map(trace =>
        StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace")
      )

    logger.info(message, arguments*)
