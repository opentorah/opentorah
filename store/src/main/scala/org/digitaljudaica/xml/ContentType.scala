package org.digitaljudaica.xml

sealed trait ContentType

object ContentType {
  final case object Empty extends ContentType
  final case object Text extends ContentType
  final case object Elements extends ContentType
  final case object Mixed extends ContentType
}
