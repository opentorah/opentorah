package org.opentorah.xml

import org.opentorah.platform.Platform
import org.opentorah.util.{Effects, Files}
import java.io.File
import java.net.URL

sealed abstract class From(val name: String, val xml: Xml):

  def isInclude: Boolean

  def url: Option[URL]

  def load: Effects.IO[xml.Element]


object From:

  private final class FromXml(override val xml: Xml)(
    name: String,
    element: xml.Element
  ) extends From(name, xml):
    override def isInclude: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    // Note: if xml != this.xml, this will fail *at run-time*:
    override def load: Effects.IO[xml.Element] = zio.ZIO.succeed(element)

  def scalaXml(name: String, elem: ScalaXml.Element): From = xml(ScalaXml)(name, elem)
  def xml(xml: Xml)(name: String, elem: xml.Element): From = FromXml(xml)(name, elem)

  private final class FromString(
    name: String,
    string: String,
    override val xml: Xml
  ) extends From(name, xml):
    override def isInclude: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: Effects.IO[xml.Element] = Effects.effect(xml.load(Sax.string2inputSource(string)))

  def string(name: String, string: String, xml: Xml = ScalaXml): From = FromString(name, string, xml)

  private final class FromUrl(fromUrl: URL, override val isInclude: Boolean, override val xml: Xml)
    extends From(Files.nameAndExtension(fromUrl.getPath)._1, xml):
    override def toString: String = s"From.url($fromUrl, isInclude=$isInclude)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: Effects.IO[xml.Element] = Effects.effect(xml.load(Sax.url2inputSource(fromUrl)))

  def url(url: URL, xml: Xml = ScalaXml): From = FromUrl(fromUrl = url, isInclude = false, xml)
  private[xml] def include(url: URL, xml: Xml): From = FromUrl(fromUrl = url, isInclude = true, xml)

  private final class FromResource(
    clazz: Class[?],
    name: String,
    override val xml: Xml
  ) extends From(name, xml):
    override def isInclude: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: Effects.IO[xml.Element] = url
      .map(url => Effects.effect(xml.load(Sax.url2inputSource(url))))
      .getOrElse(Effects.fail(s"Resource not found: $this"))

  def resourceNamed(obj: AnyRef, name: String, xml: Xml = ScalaXml): From = FromResource(obj.getClass, name, xml)
  def resource(obj: AnyRef, xml: Xml = ScalaXml): From = resourceNamed(obj, Platform.className(obj), xml)
