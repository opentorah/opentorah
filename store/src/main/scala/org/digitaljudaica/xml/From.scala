package org.digitaljudaica.xml

import java.io.{File, FileInputStream}
import java.net.URL
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

sealed trait From {

  def name: String

  def load: ErrorOr[Elem]

  final def loadDo: Elem = runA(load)

  def parse[A](parser: Parser[A]): ErrorOr[A] = Context.run(Context.complete(nest(parser)))

  def parseDo[A](parser: Parser[A]): A = runA(parse(parser))

  def nest[A](parser: Parser[A]): Parser[A] = load match {
    case Right(elem) => Context.nested(Some(this), elem, parser)
    case Left(error) => Parser.error(annotateError(error))
  }

  private def runA[A](result: ErrorOr[A]): A = result match {
    case Right(result) => result
    case Left(error) => throw new IllegalArgumentException(annotateError(error))
  }

  private def annotateError(error: String): String = s"Error in $this: $error"

  def include(url: String): From = ??? /// TODO!
}

object From {

  private final class FromXml(override val name: String, elem: Elem) extends From {
    override def toString: String = s"From.xml($name)"
    override def load: ErrorOr[Elem] = Right(elem)
  }

  def xml(name: String, elem: Elem): From = new FromXml(name, elem)

  def xml(elem: Elem): From = new FromXml("unnamed", elem)

  private final class FromUrl(url: URL) extends From {
    override def toString: String = s"From.url($url)"
    override def name: String = ??? // TODO name from the url
    override def load: ErrorOr[Elem] = loadFromUrl(url)
  }

  def url(url: URL): From = new FromUrl(url)

  private final class FromFile(file: File) extends From {
    override def name: String = ??? // TODO name from the file
    override def toString: String = s"From.file($file)"
    override def load: ErrorOr[Elem] = loadFromInputSource(new InputSource(new FileInputStream(file)))
  }

  def file(file: File): From = new FromFile(file)

  def file(directory: File, fileName: String): From = new FromFile(new File(directory, fileName + ".xml"))

  private final class FromResource(clazz: Class[_], override val name: String) extends From {
    override def toString: String = s"From.resource($clazz/$name)"
    override def load: ErrorOr[Elem] =
      Option(clazz.getResource(name + ".xml")).fold[ErrorOr[Elem]](Left("Resource not found"))(loadFromUrl)
  }

  def resource(obj: AnyRef, name: Option[String]): From =
    name.fold(resource(obj))(name => resource(obj, name))

  def resource(obj: AnyRef, name: String): From =
    new FromResource(obj.getClass, name)

  def resource(obj: AnyRef): From =
    resource(obj, org.digitaljudaica.util.Util.className(obj))

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
