package org.opentorah.collector

import org.slf4j.{Logger, LoggerFactory}

case class ServiceConfiguration(
  otherHost: String,
  port: Int
)

object ServiceConfiguration {

  def load(args: List[String]): ServiceConfiguration = ServiceConfiguration(
    otherHost = get("OTHER_HOST", "store.alter-rebbe.org"),
    port = get("PORT", "8090").toInt
  )

  private val logger: Logger = LoggerFactory.getLogger(classOf[ServiceConfiguration])

  private def get(name: String, defaultValue: String): String = {
    scala.util.Properties.envOrNone(name).fold {
      Log.notice(logger, s"No value for '$name' in the environment; using default: '$defaultValue'")
      defaultValue
    }{ value =>
      Log.notice(logger, s"Value for '$name' from the invironment: $value")
      value
    }
  }
}
