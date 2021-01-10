package org.opentorah.xml

import java.io.{File, StringReader}
import java.net.URL
import org.opentorah.util.{Files, Util}
import org.xml.sax.InputSource
import scala.xml.XML
import zio.IO

sealed abstract class From(val name: String) {

  def url: Option[URL]

  def load: IO[Error, Xml.Element]

  def isRedirect: Boolean
}

object From {

  private final class FromXml(
    name: String,
    elem: Xml.Element
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Xml.Element] = IO.succeed(elem)
  }

  def xml(name: String, elem: Xml.Element): From = new FromXml(name, elem)

  private final class FromString(
    name: String,
    string: String
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Xml.Element] = loadFromSource(new InputSource(new StringReader(string)))
  }

  def string(name: String, string: String): From = new FromString(name, string)

  private final class FromUrl(fromUrl: URL, override val isRedirect: Boolean)
    extends From(Files.nameAndExtension(fromUrl.getPath)._1)
  {
    override def toString: String = s"From.url($fromUrl, isRedirect=$isRedirect)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: IO[Error, Xml.Element] = loadFromUrl(fromUrl)
  }

  def url(url: URL): From = new FromUrl(url, false)

  private[xml] def redirect(url: URL): From = new FromUrl(url, true)

  def file(directory: File, fileName: String): From = file(new File(directory, fileName + ".xml"))
  def file(file: File): From = url(file.toURI.toURL)

  private final class FromResource(
    clazz: Class[_],
    name: String
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: IO[Error, Xml.Element] =
      url.fold[IO[Error, Xml.Element]](IO.fail(s"Resource not found: $this"))(loadFromUrl)
  }

  def resource(obj: AnyRef, name: String): From = new FromResource(obj.getClass, name)

  // def resource(obj: AnyRef, name: Option[String]): From = name.fold(resource(obj))(resource(obj, _))

  def resource(obj: AnyRef): From = resource(obj, Util.className(obj))

  private def loadFromUrl(url: URL): IO[Error, Xml.Element] = {
    if (!Files.isFileUrl(url) && !Files.isJarUrl(url)) {
      // TODO log!
      println(s"-- Loading $url")
    }
    loadFromSource(new InputSource(url.openStream()))
  }

  private val useXerces: Boolean = true

  private def loadFromSource(source: InputSource): IO[Error, Xml.Element] =
    Parser.effect(XML.loadXML(source, if (useXerces) Xerces.getParser else XML.parser))
}
