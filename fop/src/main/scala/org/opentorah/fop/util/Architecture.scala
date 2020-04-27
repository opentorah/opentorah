package org.opentorah.fop.util

sealed trait Architecture

object Architecture {
  case object i686 extends Architecture
  case object x86_64 extends Architecture
  case object amd64 extends Architecture
  case object ppc64 extends Architecture
  case object ppc64le extends Architecture
  case object s390x extends Architecture
  case object nacl extends Architecture
  case object aarch64 extends Architecture
  case object armv6l extends Architecture
  case object armv7l extends Architecture
  case object armv8l extends Architecture
}
