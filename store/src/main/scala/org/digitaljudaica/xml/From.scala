package org.digitaljudaica.xml

import java.io.File
import java.net.URL
import org.digitaljudaica.util.{Files, Util}
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

sealed abstract class From {

  def name: String

  def url: Option[URL]

  def load: ErrorOr[Elem]

  final def loadDo: Elem = From.runA(load)

  final class Parse(contentType: Content.Type) {
    def parse[A](parser: Parser[A]): ErrorOr[A] = Context.parse(Context.nested(From.this, contentType, parser))

    def parseDo[A](parser: Parser[A]): A = From.runA(parse(parser))
  }

  // TODO with Context scoped by From, maybe Xml's analogues of these can be used at top level and these removed?
  val empty = new Parse(Content.Type.Empty)
  val characters = new Parse(Content.Type.Characters)
  val elements = new Parse(Content.Type.Elements)
  val mixed = new Parse(Content.Type.Mixed)
}

object From {

  private def runA[A](result: ErrorOr[A]): A = result.fold(
    error => throw new IllegalArgumentException(error),
    result => result
  )

  private final class FromXml(
    override val name: String,
    elem: Elem
  ) extends From {
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    override def load: ErrorOr[Elem] = Right(elem)
  }

  def xml(name: String, elem: Elem): From = new FromXml(name, elem)

  def xml(elem: Elem): From = new FromXml("unnamed", elem)

  private final class FromUrl(fromUrl: URL) extends From {
    override def toString: String = s"From.url($fromUrl)"
    override def name: String = Files.nameAndExtension(fromUrl.getPath)._1
    override def url: Option[URL] = Some(fromUrl)
    override def load: ErrorOr[Elem] = loadFromUrl(fromUrl)
  }

  def url(url: URL): From = new FromUrl(url)

  def file(file: File): From = url(file.toURI.toURL)

  def file(directory: File, fileName: String): From = file(new File(directory, fileName + ".xml"))

  private final class FromResource(clazz: Class[_], override val name: String) extends From {
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: ErrorOr[Elem] =
      url.fold[ErrorOr[Elem]](Left(s"Resource not found: $this"))(loadFromUrl)
  }

  def resource(obj: AnyRef, name: String): From = new FromResource(obj.getClass, name)

  def resource(obj: AnyRef, name: Option[String]): From = name.fold(resource(obj))(resource(obj, _))

  def resource(obj: AnyRef): From = resource(obj, Util.className(obj))

  private def loadFromUrl(url: URL): ErrorOr[Elem] = Parser.toErrorOr {
    val source = new InputSource(url.openStream())
    val result = Utility.trimProper(XML.load(source))
    result.asInstanceOf[Elem]
  }

  // --- Xerces parser with Scala XML:
  // build.gradle:    implementation "xerces:xercesImpl:$xercesVersion"
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
