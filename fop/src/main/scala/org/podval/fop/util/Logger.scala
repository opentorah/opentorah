package org.podval.fop.util

trait Logger {
  def lifecycle(message: String): Unit
  def error(message: String): Unit
  def warn(message: String): Unit
  def info(message: String): Unit
  def isInfoEnabled: Boolean
  def debug(message: String): Unit
  def isDebugEnabled: Boolean
}
