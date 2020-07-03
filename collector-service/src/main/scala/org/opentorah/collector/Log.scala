package org.opentorah.collector

import net.logstash.logback.argument.StructuredArguments.keyValue
import org.http4s.Request
import org.http4s.util.CaseInsensitiveString
import org.slf4j.Logger

object Log {
  private val project: String = "alter-rebbe-2"

  def info(logger: Logger, request: Request[ServiceTask], message: String): Unit =
    log(logger, Some(request), message, "INFO")

  def notice(logger: Logger, request: Request[ServiceTask], message: String): Unit =
    log(logger, Some(request), message, "NOTICE")

  def notice(logger: Logger, message: String): Unit =
    log(logger, None, message, "NOTICE")

  def warning(logger: Logger, request: Request[ServiceTask], message: String): Unit =
    log(logger, Some(request), message, "WARNING")

  def log(logger: Logger, request: Option[Request[ServiceTask]], message: String, severity: String): Unit = {
    val trace: String = request.flatMap(getTrace).getOrElse("no-trace")

    logger.info(message,
      keyValue("severity", severity),
      keyValue("logging.googleapis.com/trace", s"projects/$project/traces/$trace"),
      null
    )
  }

  private def getTrace(request: Request[ServiceTask]): Option[String] = request
    .headers.get(CaseInsensitiveString("X-Cloud-Trace-Context"))
    .map(_.value.split("/")(0))
}
