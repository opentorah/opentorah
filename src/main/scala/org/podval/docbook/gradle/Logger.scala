package org.podval.docbook.gradle

trait Logger {
  def info(message: String): Unit
  def error(message: String): Unit
}

object Logger {
  final class PluginLogger(logger: org.gradle.api.logging.Logger) extends Logger {
    override def info(message: String): Unit = logger.info(message, null, null)
    override def error(message: String): Unit = logger.error(message)
  }

  final class TestLogger extends Logger {
    override def info(message: String): Unit = println(s"**info** $message")
    override def error(message: String): Unit = println(s"**error** $message")
  }
}
