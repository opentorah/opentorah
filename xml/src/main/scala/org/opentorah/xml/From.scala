package org.opentorah.xml

import java.io.{File, StringReader}
import java.net.URL
import org.opentorah.util.{Files, Util}
import org.xml.sax.InputSource
import scala.xml.{Elem, XML}
import zio.IO

sealed abstract class From(val name: String) {

  def url: Option[URL]

  def load: IO[Error, Elem]

  def isRedirect: Boolean
}

object From {

  private final class FromXml(
    name: String,
    elem: Elem
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Elem] = IO.succeed(elem)
  }

  def xml(name: String, elem: Elem): From = new FromXml(name, elem)

  private final class FromString(
    name: String,
    string: String
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Elem] = loadFromSource(new InputSource(new StringReader(string)))
  }

  def string(name: String, string: String): From = new FromString(name, string)

  private final class FromUrl(fromUrl: URL, override val isRedirect: Boolean)
    extends From(Files.nameAndExtension(fromUrl.getPath)._1)
  {
    override def toString: String = s"From.url($fromUrl, isRedirect=$isRedirect)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: IO[Error, Elem] = loadFromUrl(fromUrl)
  }

  def url(url: URL, isRedirect: Boolean = false): From = new FromUrl(url, isRedirect)

  def file(file: File): From = url(file.toURI.toURL)

  def file(directory: File, fileName: String): From = file(new File(directory, fileName + ".xml"))

  private final class FromResource(
    clazz: Class[_],
    name: String
  ) extends From(name) {
    override def isRedirect: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: IO[Error, Elem] =
      url.fold[IO[Error, Elem]](IO.fail(s"Resource not found: $this"))(loadFromUrl)
  }

  def resource(obj: AnyRef, name: String): From = new FromResource(obj.getClass, name)

  // def resource(obj: AnyRef, name: Option[String]): From = name.fold(resource(obj))(resource(obj, _))

  def resource(obj: AnyRef): From = resource(obj, Util.className(obj))

  private def loadFromUrl(url: URL): IO[Error, Elem] = {
    if (!Files.isFile(url) && !Files.isJar(url)) {
      // TODO log!
      println(s"-- Loading $url")
      val x = 0
    }
    loadFromSource(new InputSource(url.openStream()))
  }

  private val useXerces: Boolean = true

  private def loadFromSource(source: InputSource): IO[Error, Elem] =
    Parser.effect(XML.loadXML(source, if (useXerces) Xerces.getParser else XML.parser))
}
