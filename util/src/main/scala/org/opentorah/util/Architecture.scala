package org.opentorah.util

sealed trait Architecture {
  final def name: String = Util.className(this)
}

object Architecture {
  case object i686    extends Architecture
  case object x86_64  extends Architecture
  case object amd64   extends Architecture
  case object ppc64   extends Architecture
  case object ppc64le extends Architecture
  case object s390x   extends Architecture
  case object nacl    extends Architecture
  case object aarch64 extends Architecture
  case object armv6l  extends Architecture
  case object armv7l  extends Architecture
  case object armv8l  extends Architecture

  val values: Seq[Architecture] = Seq(i686, x86_64, amd64, ppc64, ppc64le, s390x, nacl, aarch64, armv6l, armv7l, armv8l)
}
