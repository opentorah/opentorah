package org.digitaljudaica.xml

import scala.xml.Elem

trait ToXml[A] {
  def toXml(value: A): Elem

  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)
}
