package org.opentorah.xml

sealed trait ContentType

object ContentType {
  final case object Empty extends ContentType
  final case object Characters extends ContentType
  final case object Elements extends ContentType
  final case object Mixed extends ContentType
}
