package org.digitaljudaica.xml

import java.io.{File, FileInputStream}
import java.net.URL
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

sealed trait From {

  def load: ErrorOr[Elem]

  final def loadDo: Elem = runA(load)

  def parse[A](parser: Parser[A]): ErrorOr[A] = Context.run(
    load match {
      case Right(elem) => Context.complete(Context.nested(Some(this), elem, parser))
      case Left(error) => lift(Left(s"Error in $this: $error"))
    }
  )

  def parseDo[A](parser: Parser[A]): A = runA(parse(parser))

  private def runA[A](result: ErrorOr[A]): A = result match {
    case Right(result) => result
    case Left(error) => throw new IllegalArgumentException(s"Error in $this: $error")
  }
}

object From {

  private final class FromXml(description: String, elem: Elem) extends From {
    override def load: ErrorOr[Elem] = Right(elem)
  }

  def xml(description: String, elem: Elem): From = new FromXml(description, elem)

  private final class FromUrl(url: URL) extends From {
    override def load: ErrorOr[Elem] = loadFromUrl(url)
  }

  def url(url: URL): From = new FromUrl(url)

  private final class FromFile(file: File) extends From {
    override def load: ErrorOr[Elem] = loadFromInputSource(new InputSource(new FileInputStream(file)))
  }

  def file(file: File): From = new FromFile(file)

  def file(directory: File, fileName: String): From = new FromFile(new File(directory, fileName + ".xml"))

  // TODO once metadata is generalized, make this class private and make all the methods return From, not FromResource
  final class FromResource(obj: AnyRef, name: Option[String]) extends From {
    def nameEffective: String = name.getOrElse(org.digitaljudaica.util.Util.className(obj))

    override def load: ErrorOr[Elem] = {
      val clazz = obj.getClass
      val name = nameEffective + ".xml"

      Option(clazz.getResource(name)).fold[ErrorOr[Elem]](Left("Resource not found"))(loadFromUrl)
    }
  }

  def resource(obj: AnyRef, name: Option[String]): FromResource = new FromResource(obj, name)

  def resource(obj: AnyRef): FromResource = new FromResource(obj, None)

  def resource(obj: AnyRef, name: String): FromResource = new FromResource(obj, Some(name))

  private def loadFromUrl(url: URL): ErrorOr[Elem] =
    loadFromInputSource(new InputSource(url.openStream()))

  private def loadFromInputSource(source: InputSource): ErrorOr[Elem] = try {
    Right(Utility.trimProper(XML.load(source)).asInstanceOf[Elem])
  } catch {
    case e: java.io.FileNotFoundException => Left(e.getMessage)
    case e: org.xml.sax.SAXParseException => Left(e.getMessage)
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
