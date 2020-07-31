package org.opentorah.store

import java.net.URL
import org.opentorah.xml.{Attribute, Xml}
import scala.xml.Elem

final class Redirect(
  val fromUrl: URL,
  val file: String
)

object Redirect {

  final class ToFile(
    val fromUrl: URL,
    val element: Elem
  )

  val fileAttribute: Attribute[String] = Attribute("file")

  def toFile(elementName: String, value: Redirect): ToFile = new ToFile(
    fromUrl = value.fromUrl,
    element = Xml.mkElement(
      name = elementName,
      attributes = Seq(fileAttribute.withValue(value.file)),
      content = Seq.empty
    )
  )
}
