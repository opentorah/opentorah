package org.podval.docbook.gradle

import javax.xml.transform.{ErrorListener, TransformerException}

trait Logger {
  def lifecycle(message: String): Unit
  def info(message: String): Unit
  def isInfoEnabled: Boolean
  def warn(message: String): Unit
  def error(message: String): Unit

  def errorListener: ErrorListener = new ErrorListener {
    override def warning(exception: TransformerException): Unit =
      Logger.this.warn(exception.getMessageAndLocation)
    override def error(exception: TransformerException): Unit =
      Logger.this.error(exception.getMessageAndLocation)
    override def fatalError(exception: TransformerException): Unit =
      Logger.this.error(exception.getMessageAndLocation)
  }
}

object Logger {
  final class PluginLogger(logger: org.gradle.api.logging.Logger) extends Logger {
    override def lifecycle(message: String) : Unit = logger.lifecycle(message)
    override def info(message: String): Unit = logger.info(message, null, null)
    override def isInfoEnabled: Boolean = logger.isInfoEnabled
    override def warn(message: String): Unit = logger.warn(message)
    override def error(message: String): Unit = logger.error(message)
  }
}
