package org.digitaljudaica.xml

import java.io.File
import java.net.URL
import cats.implicits._
import org.digitaljudaica.util.{Files, Util}
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

sealed trait From {

  def name: String

  def url: Option[URL]

  def load: ErrorOr[Elem]

  final def loadDo: Elem = Parser.runA(load)

  def parse[A](parser: Parser[A]): ErrorOr[A] = Context.parse(nested(parser))

  def parseDo[A](parser: Parser[A]): A = Parser.runA(parse(parser))

  private def nested[A](parser: Parser[A]): Parser[A] = load match {
    case Right(elem) => Context.nested(Some(this), elem, parser, charactersAllowed = false)
    case Left(error) => Parser.error(error)
  }
}

object From {

  private[xml] def include[A](url: String, parser: Parser[A]): Parser[A] = for {
    name <- Element.name
    currentFrom <- Context.currentFrom
    newFrom <- Parser.toParser(From.url(currentFrom.url.fold(new URL(url))(new URL(_, url))))
    result <- newFrom.nested(Element.withName(name, parser))
  } yield result

  private final class FromXml(override val name: String, elem: Elem) extends From {
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
    override def toString: String = s"From.resource($clazz/$name)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: ErrorOr[Elem] =
      url.fold[ErrorOr[Elem]](Left("Resource not found"))(loadFromUrl)
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
