package org.opentorah.xml

import java.io.{File, StringReader}
import java.net.URL
import org.opentorah.util.{Files, Util}
import org.xml.sax.InputSource
import scala.xml.{Elem, XML}
import zio.IO

sealed abstract class From {

  def name: String

  def url: Option[URL]

  def load: IO[Error, Elem]
}

object From {

  // TODO unfold?
  private final class FromXml(
    override val name: String,
    elem: Elem
  ) extends From {
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Elem] = IO.succeed(elem)
  }

  def xml(name: String, elem: Elem): From = new FromXml(name, elem)

  // TODO unfold?
  private final class FromString(
    override val name: String,
    string: String
  ) extends From {
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: IO[Error, Elem] = fromSource(new InputSource(new StringReader(string)))
  }

  def string(name: String, string: String): From = new FromString(name, string)

  // TODO unfold?
  // TODO add name parameter to From; expand subclasses; rename fromUrl parameter to url.
  private final class FromUrl(fromUrl: URL) extends From {
    override def toString: String = s"From.url($fromUrl)"
    override def name: String = Files.nameAndExtension(fromUrl.getPath)._1
    override def url: Option[URL] = Some(fromUrl)
    override def load: IO[Error, Elem] = loadFromUrl(fromUrl)
  }

  def url(url: URL): From = new FromUrl(url)

  def file(file: File): From = url(file.toURI.toURL)

  def file(directory: File, fileName: String): From = file(new File(directory, fileName + ".xml"))

  // TODO unfold?
  private final class FromResource(clazz: Class[_], override val name: String) extends From {
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: IO[Error, Elem] =
      url.fold[IO[Error, Elem]](IO.fail(s"Resource not found: $this"))(loadFromUrl)
  }

  def resource(obj: AnyRef, name: String): From = new FromResource(obj.getClass, name)

  def resource(obj: AnyRef, name: Option[String]): From = name.fold(resource(obj))(resource(obj, _))

  def resource(obj: AnyRef): From = resource(obj, Util.className(obj))

  private def loadFromUrl(url: URL): IO[Error, Elem] =
    fromSource(new InputSource(url.openStream()))

  private def fromSource(source: InputSource): IO[Error, Elem] =
    Parser.effect(XML.load(source))

  // Turns out, it wasn't the parser that eliminated whitespace from the files -
  // it was the call to Utility.trimproper!
  //
  // If for some other reason Xerces will be needed - here is how to hook it in:
  //
  // build.gradle:    implementation "xerces:xercesImpl:$xercesVersion"
  //
  //  def newSaxParserFactory: SAXParserFactory = {
  //    val result = SAXParserFactory.newInstance() // new org.apache.xerces.jaxp.SAXParserFactoryImpl
  //    result.setNamespaceAware(true)
  //    result.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
  //    //    result.setXIncludeAware(true)
  //    result
  //  }
  //
  //  def getParser: SAXParser = newSaxParserFactory.newSAXParser
  //  XML.withSAXParser(getParser) OR BETTER - XML.loadXML(InputSource, SAXParser)

  // --- XML validation with XSD; how do I do RNG?
  // https://github.com/scala/scala-xml/wiki/XML-validation
  // https://github.com/EdgeCaseBerg/scala-xsd-validation/blob/master/src/main/scala/LoadXmlWithSchema.scala
}
