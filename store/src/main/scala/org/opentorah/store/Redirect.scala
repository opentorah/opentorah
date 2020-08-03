package org.opentorah.store

import java.net.URL
import org.opentorah.xml.{Antiparser, Attribute, ToXml}
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
    element = toXml(elementName).toXml(value)
  )

  private def toXml(name: String): ToXml[Redirect] = new ToXml[Redirect] {
    override protected def elementName(value: Redirect): String = name

    override protected def antiparser: Antiparser[Redirect] = Antiparser(
      fileAttribute.toAntiparser.compose[Redirect](_.file)
    )
  }
}
