package org.podval.docbook.gradle

import org.podval.docbook.gradle.util.Logger

final class TestLogger extends Logger {
  override def lifecycle(message: String) : Unit = println(s"**lifecycle** $message")
  override def info(message: String): Unit = println(s"**info** $message")
  override def isInfoEnabled: Boolean = true
  override def warn(message: String): Unit = println(s"**warn** $message")
  override def error(message: String): Unit = println(s"**error** $message")
}
