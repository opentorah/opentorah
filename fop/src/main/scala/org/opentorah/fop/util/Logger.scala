package org.opentorah.fop.util

trait Logger {
  def error(message: String): Unit
  def warn(message: String): Unit
  def info(message: String): Unit
  def debug(message: String): Unit
}
