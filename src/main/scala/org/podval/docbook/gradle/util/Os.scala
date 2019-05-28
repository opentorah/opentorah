package org.podval.docbook.gradle.util

sealed trait Os

object Os {
  case object Windows extends Os
  case object Aix extends Os
  case object Mac extends Os
  case object FreeBSD extends Os
  case object SunOS extends Os
  case object Linux extends Os
  case object Android extends Os
}
