package org.opentorah.xml

import org.opentorah.platform.Platform
import org.opentorah.util.{Effects, Files}
import org.xml.sax.{InputSource, XMLReader}
import scala.xml.XML
import zio.ZIO
import java.io.{File, StringReader}
import java.net.URL

sealed abstract class From(val name: String):

  def isInclude: Boolean

  def url: Option[URL]

  def load: Effects.IO[Xml.Element]


object From:
  private def string2inputSource(string: String): InputSource = InputSource(StringReader(string))
  private def file2inputSource(file: File): InputSource = url2inputSource(Files.file2url(file))
  private def url2inputSource(url: URL): InputSource = InputSource(url.toString)

  private def read(
    xmlReader: XMLReader,
    inputSource: InputSource,
    fixXercesXIncludes: Boolean = true
  ): ZIO[Any, Effects.Error, Xml.Element] = for
    element: Xml.Element <- ZIO.succeed(XML.withXMLReader(xmlReader).load(inputSource))
    result <- if !fixXercesXIncludes then ZIO.succeed(element) else Xerces.fixXIncludeBug(element)
  yield result
  
  private final class FromXml(
    name: String,
    element: Xml.Element
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    // Note: if xml != this.xml, this will fail *at run-time*:
    override def load: Effects.IO[Xml.Element] = zio.ZIO.succeed(element)

  def xml(name: String, elem: Xml.Element): From = FromXml(name, elem)

  private final class FromString(
    name: String,
    string: String
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: Effects.IO[Xml.Element] = read(
      Xerces.getXMLReader(),
      string2inputSource(string)
    )

  def string(name: String, string: String): From = FromString(name, string)

  private final class FromUrl(
    fromUrl: URL,
    override val isInclude: Boolean,
    processIncludes: Xerces.ProcessIncludes = Xerces.ProcessIncludes.YesWithBases
  )
    extends From(Files.nameAndExtension(fromUrl.getPath)._1):
    override def toString: String = s"From.url($fromUrl, isInclude=$isInclude)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: Effects.IO[Xml.Element] = read(
      Xerces.getXMLReader(processIncludes = processIncludes),
      url2inputSource(fromUrl)
    )

  def url(url: URL, processIncludes: Xerces.ProcessIncludes = Xerces.ProcessIncludes.YesWithBases): From =
    FromUrl(fromUrl = url, isInclude = false, processIncludes = processIncludes)
  private[xml] def include(url: URL): From = FromUrl(fromUrl = url, isInclude = true)

  private final class FromResource(
    clazz: Class[?],
    name: String,
    fixXercesXIncludes: Boolean // This is exposed just for tests
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: Effects.IO[Xml.Element] = url
      .map(url => read(
        Xerces.getXMLReader(),
        url2inputSource(url),
        fixXercesXIncludes
      ))
      .getOrElse(Effects.fail(s"Resource not found: $this"))

  def resourceNamed(obj: AnyRef, name: String, fixXercesXIncludes: Boolean = true): From =
    FromResource(obj.getClass, name, fixXercesXIncludes)
  def resource(obj: AnyRef): From = resourceNamed(obj, Platform.className(obj))
