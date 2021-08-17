package org.opentorah.xml

sealed trait ContentType

object ContentType {
  case object Empty extends ContentType
  case object Characters extends ContentType
  case object Elements extends ContentType
  case object Mixed extends ContentType
}
