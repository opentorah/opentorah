package org.opentorah.fop.gradle

import org.gradle.api.Project
import org.opentorah.fop.util.Logger

final class PluginLogger(logger: org.gradle.api.logging.Logger) extends Logger {
  override def lifecycle(message: String) : Unit = logger.lifecycle(message)
  override def error(message: String): Unit = logger.error(message)
  override def warn(message: String): Unit = logger.warn(message)
  override def info(message: String): Unit = logger.info(message, null, null, null)
  override def isInfoEnabled: Boolean = logger.isInfoEnabled
  override def debug(message: String): Unit = logger.debug(message, null, null, null)
  override def isDebugEnabled: Boolean = logger.isDebugEnabled
}

object PluginLogger {
  def forProject(project: Project): Logger = new PluginLogger(project.getLogger)
}
