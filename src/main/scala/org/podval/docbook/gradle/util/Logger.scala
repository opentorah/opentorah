package org.podval.docbook.gradle.util

import org.gradle.api.Project

trait Logger {
  def lifecycle(message: String): Unit
  def info(message: String): Unit
  def isInfoEnabled: Boolean
  def warn(message: String): Unit
  def error(message: String): Unit
}

object Logger {
  final class PluginLogger(logger: org.gradle.api.logging.Logger) extends Logger {
    override def lifecycle(message: String) : Unit = logger.lifecycle(message)
    override def info(message: String): Unit = logger.info(message, null, null, null)
    override def isInfoEnabled: Boolean = logger.isInfoEnabled
    override def warn(message: String): Unit = logger.warn(message)
    override def error(message: String): Unit = logger.error(message)
  }

  def forProject(project: Project): Logger = new Logger.PluginLogger(project.getLogger)
}
