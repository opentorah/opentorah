package org.podval.docbook.gradle

final class TestLogger extends Logger {
  override def info(message: String): Unit = println(s"**info** $message")
  override def isInfoEnabled: Boolean = true
  override def error(message: String): Unit = println(s"**error** $message")
}
