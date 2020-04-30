package org.opentorah.fop.util

final class TestLogger extends Logger {
  override def error(message: String): Unit = println(s"**error** $message")
  override def warn(message: String): Unit = println(s"**warn** $message")
  override def info(message: String): Unit = println(s"**info** $message")
  override def debug(message: String): Unit = {} // println(s"**debug** $message")
}
