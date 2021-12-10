package org.opentorah.xml

import java.io.File
import java.net.URL
import org.opentorah.util.{Effects, Files, Platform}

sealed abstract class From(val name: String, val xml: Xml):

  def isRedirect: Boolean

  def url: Option[URL]

  def load: Effects.IO[xml.Element]


object From:

  private final class FromXml(override val xml: Xml)(
    name: String,
    element: xml.Element
  ) extends From(name, xml):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    // Note: if xml != this.xml, this will fail *at run-time*:
    override def load: Effects.IO[xml.Element] = zio.IO.succeed(element)

  def scalaXml(name: String, elem: ScalaXml.Element): From = xml(ScalaXml)(name, elem)
  def xml(xml: Xml)(name: String, elem: xml.Element): From = FromXml(xml)(name, elem)

  private final class FromString(
    name: String,
    string: String,
    override val xml: Xml
  ) extends From(name, xml):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: Effects.IO[xml.Element] = Effects.effect(xml.loadFromString(string))

  def string(name: String, string: String, xml: Xml = ScalaXml): From = FromString(name, string, xml)

  private final class FromUrl(fromUrl: URL, override val isRedirect: Boolean, override val xml: Xml)
    extends From(Files.nameAndExtension(fromUrl.getPath)._1, xml):
    override def toString: String = s"From.url($fromUrl, isRedirect=$isRedirect)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: Effects.IO[xml.Element] = Effects.effect(xml.loadFromUrl(fromUrl))

  def url(url: URL, xml: Xml): From = FromUrl(url, false, xml)

  private[xml] def redirect(url: URL, xml: Xml): From = FromUrl(url, true, xml)

  private final class FromResource(
    clazz: Class[?],
    name: String,
    override val xml: Xml
  ) extends From(name, xml):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: Effects.IO[xml.Element] = url
      .map(url => Effects.effect(xml.loadFromUrl(url)))
      .getOrElse(Effects.fail(s"Resource not found: $this"))

  def resourceNamed(obj: AnyRef, name: String, xml: Xml = ScalaXml): From = FromResource(obj.getClass, name, xml)
  def resource(obj: AnyRef, xml: Xml = ScalaXml): From = resourceNamed(obj, Platform.className(obj), xml)
